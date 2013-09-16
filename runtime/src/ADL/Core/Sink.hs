{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink(
  Sink(..),
  TransportName,
  TransportAddr
  ) where

import Data.Monoid

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import ADL.Core.Value
import ADL.Core.Primitives()
import ADL.Core.Comms.Serialisation

import ADL.Sys.Sinkimpl

-- | A @Sink@ is a reference to a processor of messages of a specified
-- type.  A @Sink@ is an instance of 'ADLValue' and hence may be
-- serialised between processes.
newtype Sink a = Sink SinkData
  deriving (Eq,Ord,Show)

instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (defaultv::a),">"]

  defaultv = Sink defaultv
  aToJSON _ (Sink d) = aToJSON jf d
  aFromJSON _ j = fmap Sink (aFromJSON jf j)

jf :: JSONFlags
jf = JSONFlags False


