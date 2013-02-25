{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink where

import qualified Data.Aeson as JSON
import qualified Data.Text as T

import ADL.Core.Value

data Sink a = NullSink
  deriving (Ord,Eq,Show)

instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (undefined::a),">"]

  defaultv = NullSink

  atoJSON flags NullSink = JSON.Null

  afromJSON flags JSON.Null = Just NullSink
