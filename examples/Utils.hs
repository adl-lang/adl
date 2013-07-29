module Utils where

import Control.Exception(throwIO,Exception)
import Control.Concurrent(threadDelay)

import ADL.Core.Comms.Rpc

threadWait :: IO ()
threadWait = threadDelay 1000 >> threadWait

errorOnTimeout :: Maybe a -> IO a
errorOnTimeout Nothing = ioError $ userError "rpc timeout"
errorOnTimeout (Just a) = return a

throwLeft :: (Exception e) => Either e a -> IO a
throwLeft (Left ev) = throwIO ev
throwLeft (Right a) = return a

throwLeftOrRight :: (Exception e1, Exception e2) => Either e1 e2 -> IO a
throwLeftOrRight (Left e1) = throwIO e1
throwLeftOrRight (Right e2) = throwIO e2

-- Handle an RPC error on the client side
throwRPCError :: RPCResult a -> IO a
throwRPCError (RPC_Error e) = throwLeftOrRight e
throwRPCError RPC_Timeout = ioError $ userError "rpc timeout"
throwRPCError (RPC_Result a) = return a

-- Handle an RPC error on the server side
throwServerRPCError :: Either RPCError a -> IO a
throwServerRPCError (Left e) = throwLeftOrRight e
throwServerRPCError (Right a) = return a

seconds :: Int -> Int
seconds n = n * 1000000