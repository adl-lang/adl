module ADL.Utils.Resource where

import Control.Exception(bracket)

-- | A resource that needs to be freed.
class Resource a where
  release :: a -> IO ()

instance (Resource a) => Resource (Either e a) where
  release (Left _) = return ()
  release (Right a) = release a

withResource :: (Resource a) => IO a -> (a -> IO b) -> IO b
withResource ma fmb = bracket ma release fmb

