{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module ADL.Core.Comms.Internals where

import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Data.Vector as V

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms.Types
import qualified ADL.Core.Comms.Null as Null

import qualified System.Log.Logger as L

data Context = Context (TVar (Map.Map TransportName Transport))

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

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (Either ConnectError (SinkConnection a))
connect (Context mv) s = do
  m <- atomically $ readTVar mv
  case Map.lookup (s_transport s) m of
    Nothing -> return (Left (TransportError NoTransport ("No transport for " ++ T.unpack (s_transport s))))
    (Just t) -> do
      c <- t_connect t (s_addr s)
      return (fmap mkSinkConnection c)

mkSinkConnection :: (ADLValue a) => Connection -> SinkConnection a
mkSinkConnection c = SinkConnection
  { sc_send = \ a -> c_send c (packMessage (aToJSON defaultJSONFlags a))
  , sc_close = c_close c
  }

newLocalSink :: forall a . (ADLValue a) => EndPoint -> Maybe SinkID -> (a -> IO ()) -> IO (LocalSink a)
newLocalSink ep msid handler =  do
     ls0 <- ep_newRawSink ep msid rawHandler
     let sink0 = ls_sink ls0
         sink = Sink (s_transport sink0) (s_addr sink0)
     return (LocalSink sink (ls_close ls0))
  where
    rawHandler :: LBS.ByteString -> IO ()
    rawHandler lbs = case parseMessage lbs of
      (Left emsg) -> L.errorM logger ("Can't parse Bytestring to JSON: " ++ emsg)
      (Right v) -> case (aFromJSON defaultJSONFlags v) of
        Nothing -> L.errorM logger ("Can't parse JSON to type " ++ T.unpack (atype (defaultv::a)))
        (Just a) -> handler a

logger = "ADL.Core.Comms.Internals"

-- Messages needs to be packed in a single element JSON array
-- so that we can carrt arbtrary JSON values.
    
packMessage :: JSON.Value -> LBS.ByteString
packMessage j = JSON.encode $ JSON.Array $ V.fromList [j]

parseMessage :: LBS.ByteString -> Either String JSON.Value
parseMessage lbs = do
  v <- JSON.eitherDecode' lbs
  case v of
    (JSON.Array v) -> case V.toList v of
      [v] -> Right v
      _ -> Left emsg
    _ -> Left emsg
  where
    emsg = "Top level JSON object must be a single element vector"

  
  