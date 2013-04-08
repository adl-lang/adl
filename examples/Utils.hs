module Utils where

import Control.Concurrent.STM
import Control.Concurrent(threadDelay)

threadWait :: IO ()
threadWait = threadDelay 1000 >> threadWait

errorOnTimeout :: Maybe a -> IO a
errorOnTimeout Nothing = ioError $ userError "rpc timeout"
errorOnTimeout (Just a) = return a

seconds :: Int -> Int
seconds n = n * 1000000