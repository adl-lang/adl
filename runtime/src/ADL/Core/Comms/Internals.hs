{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module ADL.Core.Comms.Internals where

import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Aeson as JSON

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms.Types
import ADL.Core.Comms.Serialisation
import qualified ADL.Core.Comms.Null as Null

import qualified System.Log.Logger as L

-- | A Context keeps track of all of the configured transports, and
-- makes connections to remote sinks using any of these.
--
-- Transports are implemented in additional packages, which will export
-- functions to initialise them.
data Context = Context (TVar (Map.Map TransportName Transport))

-- | Construct a new 'Context'. A program will only require
-- a single context to manage all it's communications.
newContext :: IO Context
newContext = do
    c <- fmap Context (atomically (newTVar Map.empty))
    addTransport c Null.transport
    return c

addTransport :: Context -> Transport -> IO ()
addTransport (Context mv) t = atomically $ modifyTVar mv (Map.insert (t_name t) t)

instance Resource Context where
  release (Context mv) = do
    m <- atomically $ readTVar mv
    mapM_ (release.snd) (Map.toList m)

-- | Create a new connection to a sink. The appropriate
-- transport for the sink will be used - an error will result if the
-- sinks transport has not been configured.
connect :: (ADLValue a) => Context -> Sink a -> IO (Either ConnectError (SinkConnection a))
connect (Context mv) s = do
  m <- atomically $ readTVar mv
  case Map.lookup (s_transport s) m of
    Nothing -> return (Left (TransportError NoTransport ("No transport for " ++ T.unpack (s_transport s))))
    (Just t) -> do
      ec <- t_connect t (s_addr s)
      case ec of
         Left e -> return (Left e)
         Right c -> fmap Right (mkSinkConnection (s_serialisation s) c)
      

-- | Add a boolean state variable to a connection to generate
-- errors when a send is attempted on a closed connection.
withCloseCheck :: Connection -> IO Connection
withCloseCheck (Connection send close) = do
  closedv <- atomically $ newTVar False
  return (Connection (send1 closedv) (close1 closedv))
  where
    send1 closedv lbs = do
      closed <- atomically $ readTVar closedv
      if closed then return (Left (TransportError ConnectionClosed "connection already closed"))
                else send lbs
    close1 close1v = do
      atomically $ writeTVar close1v True
      close

mkSinkConnection :: (ADLValue a) => SerialisationType -> Connection -> IO (SinkConnection a)
mkSinkConnection serType c = do
  c' <- withCloseCheck c
  return SinkConnection
    { sc_send = c_send c' . serialise serType
    , sc_close = c_close c'
    }

-- | Create a new sink.
-- @newLocalSink ep msid procf@ will create a local sink, and
-- associated it with the endpoint @ep@ If @msid@ is nothing, then a
-- unique id will be created for the association, otherwise the id
-- provided will be used. @procf@ is called whenever the sink receives a
-- value.
newLocalSink :: forall a . (ADLValue a) => EndPoint -> Maybe SinkID -> (a -> IO ()) -> IO (LocalSink a)
newLocalSink ep msid handler =  do
     (transport,addr,closef) <- ep_newRawSink ep msid rawHandler
     return (LocalSink (Sink transport addr serType) closef)
  where
    rawHandler :: LBS.ByteString -> IO ()
    rawHandler lbs = case deserialise serType lbs of
      (Left emsg) -> L.errorM logger ("Failed to deserialise message: " ++ emsg)
      (Right a) -> handler a

    serType = S_JSON

logger :: String
logger = "ADL.Core.Comms.Internals"

