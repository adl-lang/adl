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

import ADL.Examples.Kvstore2

type MapV = TVar (Map.Map T.Text T.Text)

type UserMap = Map.Map T.Text User

kvServer rfile ufile = do
    userMap <- readUserMap ufile
    mapv <- atomically $ newTVar Map.empty

    bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Left 2001)) epClose $ \ep -> do

      -- Create an actor for readonly kv requests
      readOnly <- epNewSink ep Nothing (processRequest False mapv ctx)

      -- Create an actor for read/write kv requests
      readWrite <- epNewSink ep Nothing (processRequest True mapv ctx)

      -- And finally an authentication actor to choose which 
      auth <- epNewSink ep (Just "kvstore-authenticator")
              (processAuthenticate userMap ctx (lsSink readOnly) (lsSink readWrite))

      aToJSONFile defaultJSONFlags rfile (lsSink auth)
      putStrLn ("Wrote authenticator reference to " ++ show rfile)
      threadDelay (1000*sec)

readUserMap :: FilePath -> IO UserMap
readUserMap ufile = do
    us <- aFromJSONFile' defaultJSONFlags ufile
    return (Map.fromList [(credentials_username (user_credentials u),u) | u <- us])
    

processAuthenticate :: UserMap -> Context -> KVService -> KVService -> AuthenticateReq -> IO ()
processAuthenticate userMap ctx ro rw rpc = handleRPC ctx rpc auth
  where
    auth :: Credentials -> IO (Either T.Text KVService)
    auth cred = return $ case Map.lookup (credentials_username cred) userMap of
      Nothing -> Left "Unknown username"
      (Just u) | user_credentials u == cred -> Right (if user_has_write_access u then rw else ro)
               | otherwise -> Left "incorrect password"
    
processRequest :: Bool -> MapV -> Context -> KVRequest -> IO ()
processRequest rw mapv ctx req = case req of
  (KVRequest_put rpc) -> handleRPC ctx rpc put
  (KVRequest_delete rpc) -> handleRPC ctx rpc delete
  (KVRequest_query rpc) -> handleRPC ctx rpc query
  where
    put :: KVPair -> IO (Either T.Text ())
    put (key,value)
      | rw = (atomically $ modifyTVar mapv $ Map.insert key value) >> return (Right ())
      | otherwise = return (Left "No write access")

    delete :: Key -> IO (Either T.Text ())
    delete key
      | rw = (atomically $ modifyTVar mapv $ Map.delete key) >> return (Right ())
      | otherwise = return (Left "No write access")

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
  kvServer "/tmp/kvauth.ref" "/tmp/users.json"

