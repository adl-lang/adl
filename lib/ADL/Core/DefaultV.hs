module ADL.Core.DefaultV where

import qualified Data.Text as T

class DefaultV a where
  defaultv :: a

instance DefaultV () where
  defaultv = ()

instance DefaultV Int where
  defaultv = 0

instance DefaultV Double where
  defaultv = 0

instance DefaultV T.Text where
  defaultv = T.empty

instance DefaultV [a] where
  defaultv = []
