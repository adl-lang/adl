{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Control.Applicative
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms
import ADL.Core.Comms.Rpc
import qualified ADL.Core.Comms.ZMQ as ZMQ

import ADL.Examples.Kvstore3

import Utils

type SubscID = Int

data State = State {
  kvmap :: TVar (Map.Map T.Text T.Text),
  nextid :: TVar SubscID,
  subs :: TVar (Map.Map SubscID Subscription),
  subcs :: TVar (Map.Map SubscID (Maybe (SinkConnection KVUpdate)))
  }

type CSink a = Either (Sink a) (SinkConnection a)

connectCSink :: (ADLValue a) => Context -> CSink a -> IO (CSink a)
connectCSink ctx (Left s) = do
  sc <- connect ctx s
  return (Right sc)
connectCSink _ cs@(Right _) = return cs

matchesKey :: Pattern -> Key -> Bool
matchesKey p k = T.isPrefixOf p k

matchesUpdate :: Pattern -> KVUpdate -> Bool
matchesUpdate p (KVUpdate_put (k,_)) = matchesKey p k
matchesUpdate p (KVUpdate_delete k) = matchesKey p k

kvServer rfile = do
    withResource ADL.Core.Comms.init $ \ctx -> do
    withResource (ZMQ.epOpen ctx (Left 2001)) $ \ep -> do
    withResource newState $ \state -> do
      ls <- epNewSink ep (Just "kvstore") (processRequest state ctx)
      aToJSONFile defaultJSONFlags rfile (lsSink ls)
      putStrLn ("Wrote kv server reference to " ++ show rfile)
      threadWait

newState :: IO State
newState = atomically $ State <$> newTVar Map.empty <*> newTVar 0 <*> newTVar Map.empty <*> newTVar Map.empty

instance Resource State where
  release state = do
    scmap <- atomically $ readTVar (subcs state)
    sequence_ [release s | (Just s) <- Map.elems scmap]

processRequest :: State -> Context -> KVRequest -> IO ()
processRequest state ctx req = case req of
  (KVRequest_update rpc) -> handleRPC ctx rpc update
  (KVRequest_query rpc) -> handleRPC ctx rpc query
  (KVRequest_subscribe rpc) -> handleRPC ctx rpc subscribe
  where
    update :: KVUpdate -> IO ()
    update kvupdate = do
        subsToUpdate <- atomically $ do
          modifyTVar (kvmap state) (update0 kvupdate)
          ss <- readTVar (subs state)
          return (Map.filter (\sub -> matchesUpdate (subscription_pattern sub) kvupdate) ss)
        mapM_ (sendUpdate kvupdate) (Map.toList subsToUpdate)

    sendUpdate :: KVUpdate -> (SubscID,(Pattern,Sink KVUpdate)) -> IO ()
    sendUpdate kvupdate (sid,(_,sink)) = do
      io <- atomically $ do
         cs <- readTVar (subcs state)
         case Map.lookup sid cs of
           (Just Nothing) -> retry
           (Just (Just sc)) -> return (scSend sc kvupdate)
           Nothing -> do
             writeTVar (subcs state) (Map.insert sid Nothing cs)
             return $ do
               sc <- connect ctx sink
               atomically $ writeTVar (subcs state) (Map.insert sid (Just sc) cs)
      io

    update0 (KVUpdate_put (key,value)) = Map.insert key value
    update0 (KVUpdate_delete key) = Map.delete key

    query :: Pattern -> IO QueryResults
    query p = fmap (filter (T.isPrefixOf p . fst) . Map.toList)
            $ atomically $ readTVar (kvmap state)

    subscribe :: Subscription -> IO CancelEvent
    subscribe sub = do
      atomically $ do
        i <- readTVar (nextid state)
        writeTVar (nextid state) (i+1)
        modifyTVar (subs state) (Map.update (i+1) sub)

main = do
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  kvServer "/tmp/server.ref"

