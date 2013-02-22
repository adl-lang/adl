module ADL.Core(
  module ADL.Core.DefaultV,
  module ADL.Core.JSON,
  Sink(..)
  )
  where

import ADL.Core.DefaultV
import ADL.Core.JSON

data Sink a = NullSink
  deriving (Ord,Eq,Show)

instance DefaultV (Sink a) where
  defaultv = NullSink
