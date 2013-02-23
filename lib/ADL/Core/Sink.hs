module ADL.Core.Sink where

import ADL.Core.Value
import qualified Data.Aeson as JSON


data Sink a = NullSink
  deriving (Ord,Eq,Show)

instance ADLValue (Sink a) where
  defaultv = NullSink

  atoJSON flags NullSink = JSON.Null

  afromJSON flags JSON.Null = Just NullSink
