{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Primitives (
  -- export only class instances
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Int
import Data.Word
import GHC.Float

import Data.Scientific as S

import ADL.Core.Value

instance ADLValue () where
  atype _ = "void"
  defaultv = ()

  jsonSerialiser jf = JSONSerialiser to from
    where
      to _ = JSON.Null
      from JSON.Null = Just ()
      from _ = Nothing

instance ADLValue Bool where
  atype _ = "bool"
  defaultv = False

  jsonSerialiser jf = JSONSerialiser to from
    where
      to v = JSON.Bool v
      from (JSON.Bool v) = Just v
      from _ = Nothing

iToJSON :: Integral a => a -> JSON.Value
iToJSON v = JSON.Number (fromIntegral v)

iFromJSON :: (Integral a,Bounded a) => JSON.Value -> Maybe a
iFromJSON (JSON.Number n) = case S.toBoundedInteger n of
  (Just i) -> (Just i)
  Nothing -> Nothing
iFromJSON _ = Nothing
  
instance ADLValue Int8 where
  atype _ = "int8"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Int16 where
  atype _ = "int16"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Int32 where
  atype _ = "int32"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Int64 where
  atype _ = "int64"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Word8 where
  atype _ = "word8"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Word16 where
  atype _ = "word16"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Word32 where
  atype _ = "word32"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Word64 where
  atype _ = "word64"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser iToJSON iFromJSON

instance ADLValue Double where
  atype _ = "double"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser to from
    where
      to v = JSON.Number (S.fromFloatDigits v)
      from (JSON.Number v) = Just (S.toRealFloat v)
      from  _ = Nothing

instance ADLValue Float where
  atype _ = "float"
  defaultv = 0
  jsonSerialiser jf = JSONSerialiser to from
    where
      to v = JSON.Number (S.fromFloatDigits v)
      from (JSON.Number v) = Just (S.toRealFloat v)
      from  _ = Nothing

instance ADLValue T.Text where
  atype _ = "string"
  defaultv = T.empty
  jsonSerialiser jf = JSONSerialiser to from
    where
      to v = JSON.String v
      from (JSON.String v) = Just v
      from  _ = Nothing

instance ADLValue B.ByteString where
  atype _ = "bytes"
  defaultv = B.empty
  jsonSerialiser jf = JSONSerialiser to from
    where
      to v = JSON.String (T.decodeUtf8 (B64.encode v))
      from (JSON.String v) = case B64.decode (T.encodeUtf8 v) of
        (Left _) -> Nothing
        (Right v1) -> Just v1 
      from  _ = Nothing

instance forall a . (ADLValue a) => ADLValue [a] where
  atype _ = T.concat ["vector<",atype (undefined :: a),">"]
  defaultv = []
  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to vs = JSON.Array (V.fromList (map (aToJSON js) vs))
      from (JSON.Array v) = mapM (aFromJSON js) (V.toList v)
      from _ = Nothing
  




