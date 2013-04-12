module ADL.Utils.DelayIO(
  DelayIO,
  delayIO,
  dioGet,
  dioState
  ) where

import Control.Concurrent.STM

data DelayState a = Init (IO a)
                  | Pending
                  | Ready a

type DelayIO a = TVar (DelayState a)

-- | Construct a value that will delay the running of the given IO
-- action until the first call to dioGet. dioGet can be called from
-- any thread. It's guaranteed that the io action will only be called
-- once.

delayIO :: IO a -> IO (DelayIO a)
delayIO ioa = atomically $ newTVar (Init ioa)

dioGet :: DelayIO a -> IO a
dioGet dv = do
  ioa <- atomically $ do
    s <- readTVar dv
    case s of
      (Ready a) -> return (return a)
      Pending -> retry
      (Init ioa) -> do
        writeTVar dv Pending
        return $ do
          a <- ioa
          atomically $ writeTVar dv (Ready a)
          return a
  ioa

dioState :: DelayIO a -> IO (Maybe a)
dioState dv = do
  s <- atomically $ readTVar dv
  case s of
    (Ready a) -> return (Just a)
    _ -> return Nothing
    



