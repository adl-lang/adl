{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

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

import Utils

type MapV = TVar (Map.Map T.Text T.Text)

kvServer rfile = do
    withResource ADL.Core.Comms.init $ \ctx -> do
    withResource (ZMQ.epOpen ctx (Left 2001)) $ \ep -> do
      mapv <- atomically $ newTVar Map.empty
      ls <- epNewSink ep (Just "kvstore") (processRequest mapv ctx)
      aToJSONFile defaultJSONFlags rfile (lsSink ls)
      putStrLn ("Wrote kv server reference to " ++ show rfile)
      threadWait
    
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

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  kvServer "/tmp/server.ref"

