module Main where

import Control.Exception(bracket)
import Control.Concurrent(threadDelay)
import System.Environment (getArgs)

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS

import ADL.Core.Comms
import ADL.Core.Value
import ADL.Examples.Echo


echoServer rfile = do
  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (epOpen ctx 6700) epClose $ \ep -> do
      ls <- epNewSink ep (processRequest ctx)
      let tjf = ToJSONFlags True
          lbs = (JSON.encode [(atoJSON tjf (lsSink ls))])
      LBS.writeFile rfile lbs
      putStrLn ("Wrote echo server reference to " ++ show rfile)
      threadDelay 1000000000000
  where
    processRequest :: Context -> EchoRequest () -> IO ()
    processRequest ctx req = do
      sc <- connect ctx (echoRequest_replyTo req)
      scSend sc (EchoResponse (echoRequest_body req))
  
echoClient rfile = return ()

usage = do
  putStrLn "Usage:"
  putStrLn "    echocmd server"
  putStrLn "    echocmd client"
  
main = do
  args <- getArgs
  case args of
    [] -> usage
    ["server"] -> echoServer "/tmp/server.ref"
    ["client"] -> echoClient "/tmp/server.ref"
    