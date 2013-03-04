{-# LANGUAGE ScopedTypeVariables #-}
module ADL.Core.Comms(
  Context,
  ADL.Core.Comms.init,
  close,
  EndPoint,
  epOpen,
  epNewSink,
  epClose,
  LocalSink,
  lsSink,
  lsClose,
  SinkConnection,
  connect,
  scSend,
  scClose
  ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Network.BSD

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified System.ZMQ as ZMQ
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Sink

-- | Value capturing the state of the ADL communications
-- runtime
data Context = Context {
  c_zcontext :: ZMQ.Context,
  c_connections :: TVar (Map.Map (HostName,Int) (Maybe (ZMQ.Socket ZMQ.Push,Int)))
  }

-- | Initialise the ADL communications runtime.
init :: IO Context
init = do
    zctx <- ZMQ.init 2
    cv <- atomically $ (newTVar Map.empty)
    return (Context zctx cv)

-- | Close the ADL communications runtime.
close c = ZMQ.term (c_zcontext c)

-- | To receive messages, a communications endpoint is required.
data EndPoint = EndPoint {
  ep_context :: Context,
  ep_hostname :: String,
  ep_port :: Int,
  ep_socket :: ZMQ.Socket ZMQ.Pull,
  ep_reader :: ThreadId,
  ep_sinks :: TVar (Map.Map UUID.UUID (JSON.Value -> IO ()))
}  

-- | Create a new communications endpoint, on the specifed
-- TCP port.
epOpen :: Context -> Int-> IO EndPoint
epOpen ctx port = do
  hn <- getHostName
  s <- ZMQ.socket (c_zcontext ctx) ZMQ.Pull
  ZMQ.bind s ("tcp://*:" ++ (show port))
  vsinks <- atomically $ newTVar (Map.empty)
  tid <- forkIO (reader s vsinks)
  return (EndPoint ctx hn port s tid vsinks)
  where
    reader s vsinks = loop
      where
        loop = do
          bs <- ZMQ.receive s []
          case UUID.fromByteString (LBS.fromChunks [bs]) of
            Nothing -> discard "too short for UUID"
            (Just uuid) -> do
              let bs' = BS.drop 16 bs
              case AP.parseOnly JSON.json' bs' of
                (Left emsg) -> discard ("cannot parse JSON: " ++ emsg )
                (Right v) -> do
                  sinks <- atomically $ readTVar vsinks
                  case Map.lookup uuid sinks  of
                    Nothing -> discard ("No handler for sink with UUID " ++ show uuid)
                    (Just actionf) -> do
                      Control.Exception.catch (actionf v) eHandler
          loop

    eHandler :: SomeException -> IO ()
    eHandler e = do
      L.errorM logger ("Failed to execute action:" ++ show e)
      return ()

    discard s = L.errorM logger ("Message discarded: " ++ s)

    logger = "Endpoint.reader"
      
-- | Create a new local sink from an endpoint and a message processing
-- function. The processing function will be called in an arbitrary
-- thread chosen by the ADL communications runtime.
epNewSink :: forall a . (ADLValue a) => EndPoint -> (a -> IO ()) -> IO (LocalSink a)
epNewSink ep handler = do
  uuid <- UUID.nextRandom
  let at = atype (defaultv :: a)
  atomically $ modifyTVar (ep_sinks ep) (Map.insert uuid (action at))
  return (LocalSink ep (ZMQSink (ep_hostname ep) (ep_port ep) uuid at ))
  where
    action at v = case (afromJSON fjf v) of
      Nothing -> L.errorM "Sink.action" 
          ("Message discarded: unable to parse value of type " ++ T.unpack at)
      (Just a) -> handler a

    fjf = FromJSONFlags True

-- | Close an endpoint. This implicitly closes all
-- local sinks associated with that endpoint.
epClose :: EndPoint -> IO ()
epClose ep = do
  ZMQ.close (ep_socket ep)
  killThread (ep_reader ep)

-- | A local sink is a sink where the message processing
-- is performed in this address space
data LocalSink a = LocalSink {
  ls_endpoint :: EndPoint,
  ls_sink :: Sink a
  }  

-- | Get the reference to a locally implemented sink.
lsSink :: LocalSink a -> Sink a
lsSink = ls_sink

-- | Close a local sink. No more messages will be processed.
lsClose :: LocalSink a -> IO ()
lsClose ls = atomically $
  modifyTVar (ep_sinks (ls_endpoint ls)) (Map.delete (zmqs_uuid (ls_sink ls)))

data SinkConnection a = SinkConnection {
  sc_send :: a -> IO (),
  sc_close :: IO ()
  }

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)

connect ctx NullSink = return (SinkConnection nullSend nullClose)
  where
    nullSend a = return ()
    nullClose =  return ()

connect ctx (ZMQSink{zmqs_hostname=host,zmqs_port=port,zmqs_uuid=uuid}) = do
  let key = (host,port)
  socket <- getSocket key
  return (SinkConnection (zmqSend socket) (zmqClose key) )
  where
    cmapv = c_connections ctx

    zmqSend socket a = do
      let tjf = ToJSONFlags True
          lbs = LBS.append (UUID.toByteString uuid) (JSON.encode [(atoJSON tjf a)])
      ZMQ.send' socket lbs []

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


-- | Send a message to a sink
scSend :: (ADLValue a) => SinkConnection a -> a -> IO ()
scSend sc a = sc_send sc a

-- | Close a connection
scClose :: SinkConnection a -> IO ()
scClose sc = sc_close sc

  
modifyTVar :: TVar a -> (a->a) -> STM ()
modifyTVar v f = do
  a <- readTVar v
  let a' = f a
  a' `seq` (writeTVar v a')