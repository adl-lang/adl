{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink where

import qualified Data.UUID as UUID
import qualified Data.Aeson as JSON
import qualified Data.Text as T

import ADL.Core.Value

-- | `Sink a` is a reference to a sink to which messages of type `a`
-- may be sent. Such a reference is an ADLValue and hence may be
-- serialised between processes.
data Sink a = NullSink
            | ZMQSink { zmqs_hostname :: String,
                        zmqs_port :: Int,
                        zmqs_uuid :: UUID.UUID,
                        zmqs_atype :: T.Text }
  deriving (Ord,Eq,Show)
instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (undefined::a),">"]

  defaultv = NullSink

  atoJSON flags s = JSON.String (sinkToText s)

  afromJSON flags (JSON.String s) = sinkFromText s
  afromJSON flags _ = Nothing

sinkToText :: Sink a -> T.Text
sinkToText = undefined

sinkFromText :: T.Text -> Maybe (Sink a)
sinkFromText = undefined






