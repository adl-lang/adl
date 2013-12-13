{-# LANGUAGE ScopedTypeVariables #-}
module KVClient1 where

import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import qualified ADL.Core.Comms.HTTP as HTTP

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc

import ADL.Examples.Kvstore1

import Utils

withConnection :: FilePath -> ((SinkConnection KVRequest) -> EndPoint -> IO a) -> IO a
withConnection rfile f = do
  s <- aFromJSONFile' (jsonSerialiser defaultJSONFlags) rfile 
 
  withResource newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do
      withResource (connectE ctx s) $ \sc -> do
        f sc ep

timeout = seconds 20

put key value sc ep = callRPCE KVRequest_put sc ep timeout (key,value)

delete key sc ep = callRPCE KVRequest_delete sc ep timeout key

query pattern sc ep =  do
  vs <- callRPCE KVRequest_query sc ep timeout pattern
  print vs

usage = do
  putStrLn "Usage:"
  putStrLn "    kvclient1 put <key> <value>"
  putStrLn "    kvclient1 delete <key>"
  putStrLn "    kvclient1 query <pattern>"

run args = do
  let run = withConnection "/tmp/server.ref"
  case args of
    ["put",key,value] -> run (put (T.pack key) (T.pack value))
    ["delete",key] -> run (delete (T.pack key))
    ["query",pattern] -> run (query (T.pack pattern))
    _ -> usage

main = getArgs >>= run
