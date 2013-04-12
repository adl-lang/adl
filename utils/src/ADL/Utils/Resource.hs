module ADL.Utils.Resource where

import Control.Exception(bracket)

-- | A resource that needs to be freed.
class Resource a where
  release :: a -> IO ()

withResource :: (Resource a) => IO a -> (a -> IO b) -> IO b
withResource ma fmb = bracket ma release fmb

