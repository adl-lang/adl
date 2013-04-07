{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Exception(bracket)
import System.Environment (getArgs)
import Control.Applicative
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L
import Data.Time.Clock(UTCTime,getCurrentTime,diffUTCTime)

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.ZMQ as ZMQ

import ADL.Examples.Pubsub
import ADL.Examples.Pubsub1

import Utils

type SubscID = Int

data State = State {
  nextid :: TVar SubscID,
  subs :: TVar (Map.Map SubscID (Pattern,SinkConnection Message))
  }

newState :: Context -> IO State
newState ctx = atomically $ State <$> newTVar 0 <*> newTVar Map.empty

closeState :: State -> IO ()
closeState state = do
  ss <- atomically $ readTVar (subs state)
  sequence_ [scClose sc | (_,sc) <- Map.elems ss]

psServer rfile = do
    bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (ZMQ.epOpen ctx (Left 2001)) epClose $ \ep -> do
    bracket (newState ctx) closeState $ \state -> do
      ls <- epNewSink ep (Just "pubsub") (processRequest ep state ctx)
      aToJSONFile defaultJSONFlags rfile (lsSink ls)
      putStrLn ("Wrote ps server reference to " ++ show rfile)
      threadWait

processRequest :: EndPoint -> State -> Context -> MyChannelReq -> IO ()
processRequest ep state ctx req = case req of
  (ChannelReq_publish t) -> publish t
  (ChannelReq_subscribe rpc) -> handleRPC ctx rpc subscribe
  where
    publish :: Message -> IO ()
    publish m = do
      ss <- atomically $ readTVar (subs state)
      sequence_ [ scSend sc m | (pattern,sc) <- Map.elems ss, match pattern m]

    subscribe :: MySubscribe -> IO Subscription
    subscribe req = do
      sc <- connect ctx (subscribe_sendTo req)
      i <- atomically $ do
        i <- readTVar (nextid state)
        writeTVar(nextid state) (i+1)
        modifyTVar (subs state) (Map.insert i (subscribe_pattern req,sc))
        return i
      ls <- epNewSink ep Nothing (processSubscriptionRequest state ctx i)
      return (lsSink ls)

    match :: Pattern -> Message -> Bool
    match = T.isPrefixOf

processSubscriptionRequest :: State -> Context -> Int -> SubsReq -> IO ()
processSubscriptionRequest state ctx i req = case req of
  (SubsReq_unsubscribe ()) -> unsubscribe
  where
    unsubscribe = atomically $ modifyTVar (subs state) (Map.delete i)

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  psServer "/tmp/psServer.ref"
