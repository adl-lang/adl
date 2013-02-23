module ADL.Core(
  module ADL.Core.DefaultV,
  module ADL.Core.JSON,
  Sink(..)
  )
  where

import ADL.Core.DefaultV
import ADL.Core.JSON

import qualified Data.Aeson as JSON

data Sink a = NullSink
  deriving (Ord,Eq,Show)

instance DefaultV (Sink a) where
  defaultv = NullSink

instance AToJSON (Sink a) where
  atoJSON flags NullSink = JSON.Null

instance AFromJSON (Sink a) where
  afromJSON flags JSON.Null = Just NullSink
