{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception(bracket)
import Control.Concurrent(threadDelay)
import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified System.Log.Logger as L

import ADL.Core.Comms
import qualified ADL.Core.Comms.ZMQ as ZMQ
import ADL.Core.Comms.Rpc(oneShotSinkWithTimeout)
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Examples.Echo

sec = 1000000

echoServer rfile = do
  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Left 2000)) epClose $ \ep -> do
      ls <- epNewSink ep (Just "echoserver") (processRequest ctx)
      aToJSONFile defaultJSONFlags rfile (lsSink ls)
      putStrLn ("Wrote echo server reference to " ++ show rfile)
      threadDelay (1000*sec)
  where
    processRequest :: Context -> EchoRequest () -> IO ()
    processRequest ctx req = do
      bracket (connect ctx (echoRequest_replyTo req)) scClose $ \sc -> do
        scSend sc (EchoResponse (echoRequest_body req))

echoClient rfile = do
  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Right (2100,2200))) epClose $ \ep -> do
      s <- aFromJSONFile' defaultJSONFlags rfile 
      bracket (connect ctx s) scClose $ \sc -> do 
        (sink, getValue) <- oneShotSinkWithTimeout ep (20 * sec)
        scSend sc (EchoRequest () sink)
        mv <- getValue
        case mv of
          Just (EchoResponse ()) -> putStrLn "Received response"
          Nothing -> putStrLn "Request timed out"
        return ()

usage = do
  putStrLn "Usage:"
  putStrLn "    echocmd server"
  putStrLn "    echocmd client"
  
main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  args <- getArgs
  case args of
    [] -> usage
    ["server"] -> echoServer "/tmp/server.ref"
    ["client"] -> echoClient "/tmp/server.ref"
    