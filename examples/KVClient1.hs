{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import qualified ADL.Core.Comms as AC
import qualified ADL.Core.Comms.Rpc as AC
import qualified ADL.Core.Comms.HTTP as AC

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc

import ADL.Examples.Kvstore1

import Utils

withConnection :: FilePath -> ((SinkConnection KVRequest) -> EndPoint -> IO a) -> IO a
withConnection rfile f = do
  s <- aFromJSONFile' defaultJSONFlags rfile 
 
  AC.withResource AC.newContext $ \ctx -> do
    AC.withResource (AC.newEndPoint ctx (Right (2100,2200))) $ \ep -> do
      AC.withResource (AC.connect ctx s) $ \sc -> do
        f sc ep

timeout = seconds 20


put key value sc ep = 
  callRPC KVRequest_put sc ep timeout (key,value) >>= errorOnTimeout

delete key sc ep =
  callRPC KVRequest_delete sc ep timeout key >>= errorOnTimeout

query pattern sc ep = do
  vs <- callRPC KVRequest_query sc ep timeout pattern >>= errorOnTimeout
  print vs

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