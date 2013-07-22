{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PSServer where

import System.Environment (getArgs)
import Control.Applicative
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L
import Data.Time.Clock(UTCTime,getCurrentTime,diffUTCTime)

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.HTTP as HTTP

import ADL.Examples.Pubsub
import ADL.Examples.Pubsub1

import Utils

type SubscID = Int

data State = State {
  nextid :: TVar SubscID,
  subs :: TVar (Map.Map SubscID (Pattern,SinkConnection MyMessage))
  }

newState :: Context -> IO State
newState ctx = atomically $ State <$> newTVar 0 <*> newTVar Map.empty

instance Resource State where
  release state =  do
  ss <- atomically $ readTVar (subs state)
  sequence_ [release sc | (_,sc) <- Map.elems ss]

psServer rfile = do
    withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Left 2001)) $ \ep -> do
    withResource (newState ctx)$ \state -> do
      ls <- newLocalSink ep (Just "pubsub") (processRequest ep state ctx)
      aToJSONFile defaultJSONFlags rfile (toSink ls)
      putStrLn ("Wrote ps server reference to " ++ show rfile)
      threadWait

processRequest :: EndPoint -> State -> Context -> MyChannelReq -> IO ()
processRequest ep state ctx req = case req of
  (ChannelReq_publish t) -> publish t
  (ChannelReq_subscribe rpc) -> handleRPC ctx rpc subscribe
  where
    publish :: MyMessage -> IO ()
    publish m = do
      ss <- atomically $ readTVar (subs state)
      sequence_ [ send sc m | (pattern,sc) <- Map.elems ss, match pattern m]

    subscribe :: MySubscribe -> IO Subscription
    subscribe req = do
      sc <- connect ctx (subscribe_sendTo req)
      i <- atomically $ do
        i <- readTVar (nextid state)
        writeTVar(nextid state) (i+1)
        modifyTVar (subs state) (Map.insert i (subscribe_pattern req,sc))
        return i
      ls <- newLocalSink ep Nothing (processSubscriptionRequest state ctx i)
      return (toSink ls)

    match :: Pattern -> MyMessage -> Bool
    match p m = p `T.isPrefixOf` message_payload m

processSubscriptionRequest :: State -> Context -> Int -> SubsReq -> IO ()
processSubscriptionRequest state ctx i req = case req of
  (SubsReq_unsubscribe ()) -> unsubscribe
  where
    unsubscribe = atomically $ modifyTVar (subs state) (Map.delete i)

run args =  psServer "/tmp/psServer.ref"

main = getArgs >>= run
