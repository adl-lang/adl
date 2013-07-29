{-# LANGUAGE ScopedTypeVariables #-}
module ADL.Core.Comms.Rpc(
  module ADL.Sys.Rpc,
  callRPC',
  callRPC,
  handleRPC,
  oneShotSinkWithTimeout,
  RPCResult(..),
  RPCError
  ) where

import Control.Applicative
import Control.Concurrent.STM

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms

import ADL.Sys.Rpc

type RPCError = Either ConnectError SendError

data RPCResult a = RPC_Error RPCError
                 | RPC_Timeout
                 | RPC_Result a  

-- | Make an asynchronous RPC request. The resulting IO action starts
-- the RPC request, and returns another IO action to wait for the
-- results.
callRPC :: (ADLValue i, ADLValue o, ADLValue a)
    => (Rpc i o -> a) -> SinkConnection a -> EndPoint -> Int-> i -> IO (Either RPCError (IO (Maybe o)))
callRPC selectorf sc ep timeout i = do
  (sink,waitForValue) <- oneShotSinkWithTimeout ep timeout
  es <- send sc (selectorf (Rpc i sink))
  case es of
    (Left e) -> return (Left (Right e))
    (Right ()) -> return (Right waitForValue)

-- | Make a synchronous RPC request.
callRPC' :: (ADLValue i, ADLValue o, ADLValue a)
    => (Rpc i o -> a) -> SinkConnection a -> EndPoint -> Int-> i -> IO (RPCResult o)
callRPC' selectorf sc ep timeout i = do
  ef <- callRPC selectorf sc ep timeout i
  case ef of
    (Left e) -> return (RPC_Error e)
    (Right f) -> f >>= \mo -> case mo of
      Nothing -> return RPC_Timeout
      (Just o) -> return (RPC_Result o)

-- | Respond to an RPC request on the server side
handleRPC :: (ADLValue o) => Context -> Rpc i o -> (i -> IO o) -> IO (Either RPCError ())
handleRPC ctx rpc f = do
  o <- f (rpc_params rpc)
  withResource (connect ctx (rpc_replyTo rpc)) $ \esc -> case esc of
    (Left e1) -> return (Left (Left e1))
    (Right sc) -> do
        es <- send sc o
        case es of
          (Left e2) -> return (Left (Right e2))
          (Right ()) -> return (Right ())

-- | Create a new sink to receive a value of type a. Return an IO
-- action that will wait for a value to arrive at that sink. The value
-- will be returned, unless a timeout occurs. Either way, the sink
-- will be closed.
oneShotSinkWithTimeout :: forall a . (ADLValue a) =>  EndPoint -> Int -> IO (Sink a, IO (Maybe a))
oneShotSinkWithTimeout ep timeout = do
  rv <- atomically $ newEmptyTMVar 
  ls <- newLocalSink ep Nothing (handleResponse rv)
  return (toSink ls,(getResponse rv ls))
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
