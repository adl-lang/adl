{-# LANGUAGE ScopedTypeVariables #-}
module ADL.Core.Comms.Rpc(
  module ADL.Sys.Rpc,
  callRPC,
  handleRPC,
  oneShotSinkWithTimeout
  ) where

import Control.Exception(bracket)
import Control.Applicative
import Control.Concurrent.STM

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms

import ADL.Sys.Rpc

-- | Make an RPC request from the client side. If the timeout expires,
-- return Nothing
callRPC :: (ADLValue i, ADLValue o, ADLValue a)
    => (Rpc i o -> a) -> SinkConnection a -> EndPoint -> Int-> i -> IO (Maybe o)
callRPC selectorf sc ep timeout i = do
  (sink,waitForValue) <- oneShotSinkWithTimeout ep timeout
  scSend sc (selectorf (Rpc i sink))
  waitForValue

-- | Respond to an RPC request on the server side
handleRPC :: (ADLValue o) => Context -> Rpc i o -> (i -> IO o) -> IO ()
handleRPC ctx rpc f = do
  o <- f (rpc_params rpc)
  withResource (connect ctx (rpc_replyTo rpc)) $ \sc -> do
    scSend sc o

-- | Create a new sink to receive a value of type a. Return an IO
-- action that will wait for a value to arrive at that sink. The value
-- will be returned, unless a timeout occurs. Either way, the sink
-- will be closed.
oneShotSinkWithTimeout :: forall a . (ADLValue a) =>  EndPoint -> Int -> IO (Sink a, IO (Maybe a))
oneShotSinkWithTimeout ep timeout = do
  rv <- atomically $ newEmptyTMVar 
  ls <- epNewSink ep Nothing (handleResponse rv)
  return (lsSink ls,(getResponse rv ls))
  where
    handleResponse :: TMVar a -> a -> IO ()
    handleResponse v a = atomically $ putTMVar v a

    getResponse :: TMVar a -> LocalSink a -> IO (Maybe a)
    getResponse rv ls = do
      dv <- registerDelay timeout
      r <- atomically $ (Just <$> takeTMVar rv) `orElse` (tryTimeout dv)
      release ls
      return r

    tryTimeout :: TVar Bool -> STM (Maybe a)
    tryTimeout dv = do
      v <- readTVar dv
      if v == False then retry else return Nothing
