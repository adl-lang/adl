{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink(
  Sink(..),
  sinkToText,
  sinkFromText
  ) where

import Control.Applicative

import qualified Data.UUID as UUID
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

import ADL.Core.Value

-- | `Sink a` is a reference to a sink to which messages of type `a`
-- may be sent. Such a reference is an ADLValue and hence may be
-- serialised between processes.
data Sink a = NullSink
            | ZMQSink { zmqs_hostname :: String,
                        zmqs_port :: Int,
                        zmqs_sid :: T.Text }
  deriving (Ord,Eq,Show)

instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (defaultv::a),">"]

  defaultv = NullSink

  aToJSON flags s = JSON.String (sinkToText s)

  aFromJSON flags (JSON.String s) = sinkFromText s
  aFromJSON flags _ = Nothing

sinkToText :: forall a . (ADLValue a) => Sink a -> T.Text
sinkToText NullSink = "null"
sinkToText (zmqs@ZMQSink{}) = T.concat
  [ "zmq"
  , ":", T.pack (zmqs_hostname zmqs)
  , ":", T.pack (show (zmqs_port zmqs))  
  , ":", zmqs_sid zmqs
  , ":", atype (defaultv:: a)
  ]  

sinkFromText :: (ADLValue a) => T.Text -> Maybe (Sink a)
sinkFromText t = case A.parseOnly sinkP t of
  (Left _) -> Nothing
  (Right s) -> return s

sinkP :: forall a . (ADLValue a) => A.Parser (Sink a)
sinkP =   A.string "null" *> (pure NullSink)
          <|> A.string "zmq"  *> (ZMQSink <$> (T.unpack <$> cl field) <*> cl A.decimal <*> cl field) <* cl atype1
  where
    atype1 = A.string (atype (defaultv::a))

    cl p = A.char ':' *> p

    field :: A.Parser T.Text
    field = A.takeWhile (/=':')


