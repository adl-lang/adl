module Main where

import System.Environment (getArgs)
import Data.List(find)

import qualified System.Log.Logger as L

import qualified KVServer1
import qualified KVClient1
import qualified KVServer2
import qualified KVClient2
import qualified PSServer
import qualified PSClient
import qualified Echo
import qualified Serialisation

examples  =
  [ ("echoserver", Echo.runserver)
  , ("echoclient", Echo.runclient)
  , ("kvserver1", KVServer1.run)
  , ("kvclient1", KVClient1.run)
  , ("kvserver2", KVServer2.run)
  , ("kvclient2", KVClient2.run)
  , ("psserver", PSServer.run)
  , ("psclient", PSClient.run)
  , ("serialise-test-server", Serialisation.runserver)
  , ("serialise-test-client", Serialisation.runclient)
  ]
            
usage = do
  putStrLn "Usage:"
  mapM_ (\(name,_) -> putStrLn ("    runexample [--debug] " ++ name ++ " <args> ...")) examples
  
main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)

  args <- getArgs
  args' <- case args of
    ("--debug":args) -> do
      L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
      return args
    args -> return args
      
  case args' of
    [] -> usage
    (name:args') -> case find (\(name1,_) -> name == name1) examples of
      Nothing -> usage
      (Just (_,runfn)) -> runfn args'
