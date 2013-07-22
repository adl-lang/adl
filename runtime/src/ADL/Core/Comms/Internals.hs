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
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)
connect (Context mv) s = do
  m <- atomically $ readTVar mv
  case Map.lookup (s_transport s) m of
    Nothing -> error ("(No transport for " ++ T.unpack (s_transport s))
    (Just t) -> do
      c <- t_connect t (s_addr s)
      return (mkSinkConnection c)

mkSinkConnection :: (ADLValue a) => Connection -> SinkConnection a
mkSinkConnection c = SinkConnection
  { sc_send = \ a -> c_send c (packMessage (aToJSON defaultJSONFlags a))
  , sc_close = c_close c
  }

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

  
  