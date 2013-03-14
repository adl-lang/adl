{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception(bracket)
import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.ZMQ as ZMQ

import ADL.Examples.Kvstore1


withConnection :: FilePath -> ((SinkConnection KVRequest) -> EndPoint -> IO a) -> IO a
withConnection rfile f = do
  (Just s) <- fmap sinkFromText (T.readFile rfile)
--  bracket ADL.Core.Comms.init close $ \ctx -> do
  bracket ADL.Core.Comms.init (const (return ())) $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Left 6701)) epClose $ \ep -> do
      bracket (connect ctx s) scClose $ \sc -> do
        f sc ep

timeout = 20 * 1000000

put key value sc ep = do
  callRPC KVRequest_put sc ep timeout (key,value)
  return ()

delete key sc ep = do
  callRPC KVRequest_delete sc ep timeout key
  return ()

query pattern sc ep = do
  mvs <- callRPC KVRequest_query sc ep timeout pattern
  print mvs
  return ()
  

usage = do
  putStrLn "Usage:"
  putStrLn "    kvclient1 put <key> <value>"
  putStrLn "    kvclient1 delete <key>"
  putStrLn "    kvclient1 query <pattern>"

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  args <- getArgs
  let run = withConnection "/tmp/server.ref"
  case args of
    ["put",key,value] -> run (put (T.pack key) (T.pack value))
    ["delete",key] -> run (delete (T.pack key))
    ["query",pattern] -> run (query (T.pack pattern))
    _ -> usage