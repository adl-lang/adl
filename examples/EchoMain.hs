{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative
import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified System.Log.Logger as L

import ADL.Utils.Resource
import ADL.Core.Comms
import qualified ADL.Core.Comms.HTTP as EP
import ADL.Core.Comms.Rpc(oneShotSinkWithTimeout)
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Examples.Echo

import Utils

echoServer rfile = do
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    withResource (EP.newEndPoint ctx (Left 2000)) $ \ep -> do
      ls <- newLocalSink ep (Just "echoserver") (processRequest ctx)
      aToJSONFile defaultJSONFlags rfile (toSink ls)
      putStrLn ("Wrote echo server reference to " ++ show rfile)
      threadWait
  where
    processRequest :: Context -> EchoRequest () -> IO ()
    processRequest ctx req = do
      withResource (connect ctx (echoRequest_replyTo req)) $ \sc -> do
        send sc (EchoResponse (echoRequest_body req))

echoClient rfile = do
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    withResource (EP.newEndPoint ctx (Right (2100,2200))) $ \ep -> do
      s <- aFromJSONFile' defaultJSONFlags rfile 
      withResource (connect ctx s) $ \sc -> do 
        (sink, getValue) <- oneShotSinkWithTimeout ep (seconds 20)
        send sc (EchoRequest () sink)
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
    