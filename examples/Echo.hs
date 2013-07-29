{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Echo where

import Control.Monad(void)
import System.Environment (getArgs)

import qualified System.Log.Logger as L

import ADL.Utils.Resource
import ADL.Core.Comms
import qualified ADL.Core.Comms.HTTP as HTTP
import ADL.Core.Comms.Rpc(oneShotSinkWithTimeout)
import ADL.Core.Value
import ADL.Examples.Echo

import Utils

echoServer rfile =
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Left 2000)) $ \ep -> do
      ls <- newLocalSink ep (Just "echoserver") (processRequest ctx)
      aToJSONFile defaultJSONFlags rfile (toSink ls)
      putStrLn ("Wrote echo server reference to " ++ show rfile)
      threadWait
  where
    processRequest :: Context -> EchoRequest () -> IO ()
    processRequest ctx req =
      withResource (throwLeft =<< connect ctx (echoRequest_replyTo req)) $ \sc ->
        void $ send sc (EchoResponse (echoRequest_body req))

echoClient rfile =
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do
      s <- aFromJSONFile' defaultJSONFlags rfile 
      withResource (throwLeft =<< connect ctx s) $ \sc -> do 
        (sink, getValue) <- oneShotSinkWithTimeout ep (seconds 20)
        void $ send sc (EchoRequest () sink)
        mv <- getValue
        case mv of
          Just (EchoResponse ()) -> putStrLn "Received response"
          Nothing -> putStrLn "Request timed out"
        return ()

runserver args = echoServer "/tmp/server.ref"
runclient args = echoClient "/tmp/server.ref"

usage = do
  putStrLn "Usage:"
  putStrLn "    echocmd server"
  putStrLn "    echocmd client"
  
main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  args <- getArgs
  case args of
    [] -> usage
    ("server":args') -> runserver args'
    ("client":args') -> runclient args'
    