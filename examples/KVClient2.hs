{-# LANGUAGE ScopedTypeVariables #-}
module KVClient2 where

import Control.Monad
import System.Environment (getArgs,getEnv)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.HTTP as HTTP

import ADL.Examples.Kvstore2

import Utils

withConnection :: FilePath -> Credentials -> ((SinkConnection KVRequest) -> EndPoint -> IO a) -> IO a
withConnection rfile cred f = do
  s <- aFromJSONFile' defaultJSONFlags rfile 
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do

      -- Connect and to the authenticator and get a reference to the kvservice
      withResource (connect ctx s) $ \sc -> do
        kv <- throwOnError =<< throwOnTimeout =<< callRPC id sc ep timeout cred

        -- Connect to the kvservice
        withResource (connect ctx kv) $ \sc -> do

          -- and run the action
          f sc ep

timeout = seconds 20

throwOnTimeout :: Maybe a -> IO a
throwOnTimeout Nothing = ioError $ userError "rpc timeout"
throwOnTimeout (Just a) = return a

throwOnError :: Either T.Text a -> IO a
throwOnError (Left e) = ioError $ userError (T.unpack e)
throwOnError (Right a) = return a

put key value sc ep = 
  throwOnError =<< throwOnTimeout =<< callRPC KVRequest_put sc ep timeout (key,value)

delete key sc ep =
  throwOnError =<< throwOnTimeout =<< callRPC KVRequest_delete sc ep timeout key

query pattern sc ep = do
  vs <- throwOnTimeout  =<< callRPC KVRequest_query sc ep timeout pattern
  print vs

credentialsFromEnv :: IO Credentials
credentialsFromEnv = do
  user <- getEnv "KV_USER"
  password <- getEnv "KV_PASSWORD"
  return (Credentials (T.pack user) (T.pack password))

usage = do
  putStrLn "Usage:"
  putStrLn "    kvclient2 put <key> <value>"
  putStrLn "    kvclient2 delete <key>"
  putStrLn "    kvclient2 query <pattern>"
  putStrLn ""
  putStrLn "(the environment variable KV_USER and KV_PASSWORD must be set)"

run args = do
  cred <- credentialsFromEnv
  let run = withConnection "/tmp/kvauth.ref" cred
  case args of
    ["put",key,value] -> run (put (T.pack key) (T.pack value))
    ["delete",key] -> run (delete (T.pack key))
    ["query",pattern] -> run (query (T.pack pattern))
    _ -> usage

main = getArgs >>= run