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

import ADL.Examples.Pubsub
import ADL.Examples.Pubsub1

import Utils

withConnection :: FilePath -> ((SinkConnection MyChannelReq) -> EndPoint -> IO a) -> IO a
withConnection rfile f = do
  s <- aFromJSONFile' defaultJSONFlags rfile 

  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Right (2100,2200))) epClose $ \ep -> do
      bracket (connect ctx s) scClose $ \sc -> do
        f sc ep

publish :: Message -> SinkConnection MyChannelReq -> EndPoint -> IO ()
publish value sc ep = scSend sc (ChannelReq_publish value)

subscribe :: Pattern -> SinkConnection MyChannelReq -> EndPoint -> IO ()
subscribe pattern sc ep = do
  bracket (epNewSink ep Nothing processMessage) lsClose $ \ls -> do
  sub <- callRPC ChannelReq_subscribe sc ep (seconds 20) (Subscribe pattern (lsSink ls)) >>= errorOnTimeout
  threadWait
  where
    processMessage :: Message -> IO ()
    processMessage m = T.putStrLn m

usage = do
  putStrLn "Usage:"
  putStrLn "    psclient publish <value>"
  putStrLn "    psclient subscribe <pattern>"

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  args <- getArgs
  let run = withConnection "/tmp/psServer.ref"
  case args of
    ["publish",value] -> run (publish (T.pack value))
    ["subscribe",pattern] -> run (subscribe (T.pack pattern))
    _ -> usage