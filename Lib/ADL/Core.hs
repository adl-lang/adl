module ADL.Core(
  module ADL.Core.DefaultV,
  AToJSON(..),
  AFromJSON(..),
  Sink(..)
  )
  where

import ADL.Core.DefaultV

import qualified Data.Aeson as JSON

class AToJSON a where
  atoJSON :: a -> JSON.Value

class AFromJSON a where
  afromJSON :: JSON.Value -> Maybe a

data Sink a = NullSink
  deriving (Ord,Eq,Show)

instance DefaultV (Sink a) where
  defaultv = NullSink
