module Utils where

import Control.Concurrent.STM
import Control.Concurrent(threadDelay)

threadWait :: IO ()
threadWait = threadDelay 1000 >> threadWait

modifyTVar :: TVar a -> (a->a) -> STM ()
modifyTVar v f = do
  a <- readTVar v
  let a' = f a
  a' `seq` (writeTVar v a')


errorOnTimeout :: Maybe a -> IO a
errorOnTimeout Nothing = ioError $ userError "rpc timeout"
errorOnTimeout (Just a) = return a

seconds :: Int -> Int
seconds n = n * 1000000