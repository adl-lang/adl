{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module ADL.Core.Comms.ZMQ.Internals(
  Context,
  ADL.Core.Comms.ZMQ.Internals.init,
  close,
  connect,
  EndPointData,
  epOpen
  ) where

import Data.Monoid
import Control.Exception
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Network.BSD(getHostName,HostName)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified System.ZMQ3 as ZMQ
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Sink

import ADL.Core.Comms.Types

-- | Value capturing the state of the ADL ZMQ communications
-- runtime
data Context = Context {
  c_zcontext :: ZMQ.Context,
  c_connections :: TVar (Map.Map (HostName,Int) (Maybe (ZMQ.Socket ZMQ.Push,Int)))
  }

-- | Initialise the ZMQ communications runtime.
init :: IO Context
init = do
    zctx <- ZMQ.context
    cv <- atomically $ (newTVar Map.empty)
    return (Context zctx cv)

-- | Close the ZMQ communications runtime.
close c = do
  L.debugM "ZMQ.destroy" "starting ZMQ.destroy..."
  ZMQ.destroy (c_zcontext c)
  L.debugM "ZMQ.destroy" "ZMQ.destroy done"

-- | To receive messages, a communications endpoint is required.
data EndPointData = EndPointData {
  ep_context :: Context,
  ep_hostname :: HostName,
  ep_port :: Int,
  ep_socket :: ZMQ.Socket ZMQ.Pull,
  ep_reader :: ThreadId,
  ep_sinks :: TVar (Map.Map T.Text (JSON.Value -> IO ()))
}  

-- | Create a new communications endpoint, on the specifed
-- TCP port.
epOpen :: Context -> Int -> IO EndPoint
epOpen ctx port = do
  ed <- epOpen1 ctx port
  return (epCreate (epNewSink1 ed) (epClose1 ed))

epOpen1 :: Context -> Int-> IO EndPointData
epOpen1 ctx port = do
  hn <- getHostName
  s <- ZMQ.socket (c_zcontext ctx) ZMQ.Pull
  ZMQ.bind s ("tcp://*:" ++ (show port))
  sinksv <- atomically $ newTVar (Map.empty)
  nextactionv <- atomically $ newEmptyTMVar
  tid <- forkIO (reader s sinksv nextactionv)
  forkIO (runner nextactionv)
  return (EndPointData ctx hn port s tid sinksv)
  where
    -- The reader thread reads messages from the transport, parses
    -- them and pushes them to the runner. This thread expects to be
    -- killed when the endpoint is closed.
    reader s sinksv nextactionv = Control.Exception.handleJust isThreadKilled (threadKilledHandler s nextactionv) loop 
      where
        loop = do 
          bs <- ZMQ.receive s
          L.debugM logger ("Received message body:" ++ show bs)
          case parseMessage (LBS.fromChunks [bs]) of
            (Left emsg) -> discard ("cannot parse message header & JSON body: " ++ emsg )
            (Right (sid,v)) -> do
              sinks <- atomically $ readTVar sinksv
              case Map.lookup sid sinks  of
                Nothing -> discard ("No handler for sink with SID " ++ show sid)
                (Just actionf) -> do
                  atomically $ putTMVar nextactionv (Just (actionf v))
          loop

    isThreadKilled ThreadKilled = Just ThreadKilled
    isThreadKilled _ = Nothing

    threadKilledHandler s nextactionv e = do
      ZMQ.close s
      atomically $ putTMVar nextactionv Nothing

    -- The runner thread executes the actions. This thread will never
    -- be killed (actions always run to completion). It shuts down
    -- when signalled via a Nothing value.
    runner nextactionv = loop
      where
        loop = do
          ma <- atomically $ takeTMVar nextactionv
          case ma of
            Nothing -> return ()
            (Just action) -> do
              Control.Exception.handle actionExceptionHandler action
              loop

    actionExceptionHandler :: SomeException -> IO ()
    actionExceptionHandler e = do
      L.errorM logger ("Failed to execute action:" ++ show e)
      return ()


    discard s = L.errorM logger ("Message discarded: " ++ s)

    logger = "Endpoint"

-- | Create a new local sink from an endpoint and a message processing
-- function. The processing function will be called in an arbitrary
-- thread chosen by the ADL communications runtime.
epNewSink1 :: forall a . (ADLValue a) => EndPointData -> (a -> IO ()) -> IO (LocalSink a)
epNewSink1 ep handler = do
  uuid <- fmap (T.pack . UUID.toString) UUID.nextRandom
  let at = atype (defaultv :: a)
      sink = ZMQSink (ep_hostname ep) (ep_port ep) uuid
  atomically $ modifyTVar (ep_sinks ep) (Map.insert uuid (action at))
  return (lsCreate sink (closef sink))
  where
    action at v = case (afromJSON fjf v) of
      Nothing -> L.errorM "Sink.action" 
          ("Message discarded: unable to parse value of type " ++ T.unpack at)
      (Just a) -> handler a

    closef sink = atomically $ modifyTVar (ep_sinks ep) (Map.delete (zmqs_sid sink))

    fjf = FromJSONFlags True

-- | Close an endpoint. This implicitly closes all
-- local sinks associated with that endpoint.
epClose1 :: EndPointData -> IO ()
epClose1 ep = killThread (ep_reader ep)


-- A message is a sink id and a json value. We represent this on the
-- wire as a 2 element JSON array.
type Message = (T.Text,JSON.Value)

packMessage :: Message -> LBS.ByteString
packMessage (sid,v) = JSON.encode (JSON.Array(V.fromList [JSON.String sid,v]))

parseMessage :: LBS.ByteString -> Either String Message
parseMessage lbs = do
  v <- JSON.eitherDecode' lbs
  case v of
    (JSON.Array v) -> case V.toList v of
      [JSON.String sid,v] -> Right (sid,v)
      _ -> Left msg
  where
    msg = "Top level JSON object must be a two element vector"

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)

connect ctx (ZMQSink{zmqs_hostname=host,zmqs_port=port,zmqs_sid=sid}) = do
  let key = (host,port)
  socket <- getSocket key
  return (scCreate (zmqSend socket) (zmqClose key) )
  where
    cmapv = c_connections ctx

    zmqSend socket a = do
      let tjf = ToJSONFlags True
          lbs = packMessage (sid,atoJSON tjf a)
      L.debugM logger ("Sending message to " ++ host ++ "/" ++ show port ++ ":" ++ show lbs)
      ZMQ.send' socket [] lbs

    zmqClose key = atomically $ do
        cs <- readTVar cmapv
        case Map.lookup key cs of
          Just (Just (socket,0)) -> do
            writeTVar cmapv (Map.delete key cs)
          Just (Just (socket,refs)) -> do
            writeTVar cmapv (Map.insert key (Just (socket,refs-1)) cs)
          _ -> return ()
    
    getSocket key = do
      ms <- atomically $ do
        cs <- readTVar cmapv
        case Map.lookup key cs of
          -- We have a connection
          Just (Just (socket,refs)) -> do
            writeTVar cmapv (Map.insert key (Just (socket,refs+1)) cs)
            return (Just socket)

          -- A connection is currently being created
          Just Nothing -> retry

          -- We need to create a new connection
          Nothing -> do
            writeTVar cmapv (Map.insert key Nothing cs)
            return Nothing
      case ms of
        (Just socket) -> return socket
        Nothing -> do
          socket <- ZMQ.socket (c_zcontext ctx) ZMQ.Push
          let (host,port) = key
          ZMQ.connect socket ("tcp://" ++ host ++ ":" ++ show port)
          atomically $ modifyTVar cmapv (Map.insert key (Just (socket,1)))
          return socket

    logger = "SinkConnection.zmqSend"


modifyTVar :: TVar a -> (a->a) -> STM ()
modifyTVar v f = do
  a <- readTVar v
  let a' = f a
  a' `seq` (writeTVar v a')