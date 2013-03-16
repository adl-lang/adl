{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Exception(bracket)
import Control.Concurrent(threadDelay)
import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.ZMQ as ZMQ

import ADL.Examples.Kvstore1


type MapV = TVar (Map.Map T.Text T.Text)

kvServer rfile = do
    bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Left 2001)) epClose $ \ep -> do
      mapv <- atomically $ newTVar Map.empty
      ls <- epNewSink ep (Just "kvstore") (processRequest mapv ctx)
      T.writeFile rfile (sinkToText (lsSink ls))
      putStrLn ("Wrote kv server reference to " ++ show rfile)
      threadDelay (1000*sec)
    
processRequest :: MapV -> Context -> KVRequest -> IO ()
processRequest mapv ctx req = case req of
  (KVRequest_put rpc) -> handleRPC ctx rpc put
  (KVRequest_delete rpc) -> handleRPC ctx rpc delete
  (KVRequest_query rpc) -> handleRPC ctx rpc query
  where
    put :: KVPair -> IO ()
    put (key,value) = atomically $ modifyTVar mapv $ Map.insert key value

    delete :: Key -> IO ()
    delete key = atomically $ modifyTVar mapv $ Map.delete key

    query :: Pattern -> IO QueryResults
    query p = fmap (filter (T.isPrefixOf p . fst) . Map.toList)
            $ atomically $ readTVar mapv

modifyTVar :: TVar a -> (a->a) -> STM ()
modifyTVar v f = do
  a <- readTVar v
  let a' = f a
  a' `seq` (writeTVar v a')

sec = 1000000

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  kvServer "/tmp/server.ref"

