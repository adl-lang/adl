{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module PSClient where

import Control.Monad(void)
import Control.Exception(bracket)
import System.Environment (getArgs)
import System.Posix.Signals
import Data.Time.Clock(getCurrentTime)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import ADL.Utils.Resource
import ADL.Utils.Format
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.HTTP as HTTP

import ADL.Examples.Pubsub
import ADL.Examples.Pubsub1

import Utils

rfile = "/tmp/psServer.ref"

withConnection :: FilePath -> (Context -> SinkConnection MyChannelReq -> HTTP.Transport -> IO a) -> IO a
withConnection rfile f = do
  s <- aFromJSONFile' (jsonSerialiser defaultJSONFlags) rfile 
  withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (connectE ctx s) $ \sc ->
      f ctx sc http

publish :: Payload-> IO ()
publish payload = do
  withConnection rfile $ \ctx sc http -> do
    tstamp <- getCurrentTime
    sendE sc (ChannelReq_publish (Message tstamp payload))

subscribe :: Pattern -> IO ()
subscribe pattern =
  withConnection rfile $ \ctx sc http -> do
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do
      withResource (newLocalSink ep Nothing processMessage) $ \ls -> do
        s <- callRPCE ChannelReq_subscribe sc ep (seconds 20) (Subscribe pattern (toSink ls))
        waitForSigINT
        withResource (connectE ctx s) $ \sc1 ->
          sendE sc1 (SubsReq_unsubscribe ())
  where
    processMessage :: MyMessage -> IO ()
    processMessage m = T.putStrLn (template "$1: $2" [T.pack (show (message_timestamp m)),message_payload m])

waitForSigINT :: IO ()
waitForSigINT = do
  v <- atomically $ newEmptyTMVar
  bracket (installHandler sigINT (Catch $ atomically $ putTMVar v ()) Nothing)
          (\h-> installHandler sigINT h Nothing)
          (const $ atomically $ takeTMVar v)

usage = do
  putStrLn "Usage:"
  putStrLn "    psclient publish <value>"
  putStrLn "    psclient subscribe <pattern>"

run ["publish",value] = publish (T.pack value)
run ["subscribe",pattern] = subscribe (T.pack pattern)
run _ = usage

main = getArgs >>= run
