{-# LANGUAGE ScopedTypeVariables #-}
module ADL.Core.Comms.Rpc(
  oneShotSinkWithTimeout
  ) where

import Control.Applicative
import Control.Concurrent.STM

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms

import ADL.Sys.Rpc

rpc :: (ADLValue i, ADLValue o) => (Rpc i o -> IO ()) -> EndPoint -> Int-> i -> IO (Maybe o)
rpc sendf ep timeout i = do
  (sink,waitForValue) <- oneShotSinkWithTimeout ep timeout
  sendf (Rpc i sink)
  waitForValue

-- | Create a new sink to receive a value of type a. Return an IO
-- action that will wait for a value to arrive at that sink. The value
-- will be returned, unless a timeout occurs. Either way, the sink
-- will be closed.
oneShotSinkWithTimeout :: forall a . (ADLValue a) =>  EndPoint -> Int -> IO (Sink a, IO (Maybe a))
oneShotSinkWithTimeout ep timeout = do
  rv <- atomically $ newEmptyTMVar 
  ls <- epNewSink ep (handleResponse rv)
  return (lsSink ls,(getResponse rv ls))
  where
    handleResponse :: TMVar a -> a -> IO ()
    handleResponse v a = atomically $ putTMVar v a

    getResponse :: TMVar a -> LocalSink a -> IO (Maybe a)
    getResponse rv ls = do
      dv <- registerDelay timeout
      r <- atomically $ (Just <$> takeTMVar rv) `orElse` (tryTimeout dv)
      lsClose ls
      return r

    tryTimeout :: TVar Bool -> STM (Maybe a)
    tryTimeout dv = do
      v <- readTVar dv
      if v == False then retry else return Nothing
