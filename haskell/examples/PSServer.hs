{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PSServer where

import System.Environment (getArgs)
import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe(catMaybes)
import Control.Monad(when)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L
import Data.Time.Clock(UTCTime,getCurrentTime,diffUTCTime)

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

logger :: String
logger = "PSServer"

infoM :: T.Text -> [T.Text] -> IO ()
infoM t vs = L.infoM logger $ T.unpack $ template t vs

psServer rfile =
    withResource ADL.Core.Comms.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Left 2001)) $ \ep -> do
    withResource (newState ctx)$ \state -> do
    ls <- newLocalSink ep (Just "pubsub") (processRequest ep state ctx)
    aToJSONFile (jsonSerialiser defaultJSONFlags) rfile (toSink ls)
    infoM "Wrote ps server reference to $1" [T.pack rfile]
    threadWait

processRequest :: EndPoint -> State -> Context -> MyChannelReq -> IO ()
processRequest ep state ctx req = case req of
  (ChannelReq_publish t) -> publish t
  (ChannelReq_subscribe rpc) -> throwServerRPCError =<< handleRPC ctx rpc subscribe
  where
    publish :: MyMessage -> IO ()
    publish m = do
      ss <- atomically $ readTVar (subs state)
      mfailures <- sequence [ sendm i sc m | (i,(pattern,sc)) <- Map.toList ss, match pattern m]
      let failures = catMaybes mfailures
      when (not (null failures)) $ 
          infoM "Removing subscriber(s) $1 due to comms failures" [fshow failures]
      atomically $ modifyTVar (subs state) (deleteN failures)

    sendm i sc m = send sc m >>= \ee -> case ee of
        (Left e) -> return (Just i)
        (Right ()) -> return Nothing

    deleteN :: (Ord k) => [k] -> Map.Map k v -> Map.Map k v
    deleteN ks m = foldr Map.delete m ks

    subscribe :: MySubscribe -> IO Subscription
    subscribe req = do
      esc <- connect ctx (subscribe_sendTo req)
      case esc of
        (Left e) -> error ("Failed to connect:" ++ show e)
        (Right sc) -> do
          let pat = subscribe_pattern req
          i <- atomically $ do
            i <- readTVar (nextid state)
            writeTVar(nextid state) (i+1)
            modifyTVar (subs state) (Map.insert i (pat,sc))
            return i
          ls <- newLocalSink ep Nothing (processSubscriptionRequest state ctx i)
          infoM "Added new subscriber #$1 for pattern $2" [fshow i,pat]
          return (toSink ls)

    match :: Pattern -> MyMessage -> Bool
    match p m = p `T.isPrefixOf` message_payload m

processSubscriptionRequest :: State -> Context -> Int -> SubsReq -> IO ()
processSubscriptionRequest state ctx i req = case req of
  SubsReq_unsubscribe -> unsubscribe
  where
    unsubscribe = do
        infoM "Removed subscriber #$1" [fshow i]
        atomically $ modifyTVar (subs state) (Map.delete i)

run args =  psServer "/tmp/psServer.ref"

main = getArgs >>= run
