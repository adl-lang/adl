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

import Data.Attoparsec.Number

import ADL.Core.Value

instance ADLValue () where
  atype _ = "void"
  
  defaultv = ()

  aToJSON _ () = JSON.Null

  aFromJSON _ JSON.Null = Just ()
  aFromJSON _ _ = Nothing

instance ADLValue Bool where
  atype _ = "bool"

  defaultv = False

  aToJSON _ v = JSON.Bool v

  aFromJSON _ (JSON.Bool v) = Just v
  aFromJSON _ _ = Nothing


iToJSON _ v = JSON.Number (I (fromIntegral v))
iFromJSON _ (JSON.Number (I v)) = Just (fromIntegral v)
iFromJSON _ _ = Nothing

instance ADLValue Int8 where
  atype _ = "int8"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Int16 where
  atype _ = "int16"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Int32 where
  atype _ = "int32"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Int64 where
  atype _ = "int64"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Word8 where
  atype _ = "uint8"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Word16 where
  atype _ = "uint16"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Word32 where
  atype _ = "uint32"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Word64 where
  atype _ = "uint64"
  defaultv = 0
  aToJSON = iToJSON
  aFromJSON = iFromJSON

instance ADLValue Double where
  atype _ = "double"
  defaultv = 0
  aToJSON _ v = JSON.Number (D v)
  aFromJSON _ (JSON.Number (D v)) = Just v
  aFromJSON _ _ = Nothing

instance ADLValue Float where
  atype _ = "float"
  defaultv = 0
  aToJSON _ v = JSON.Number (D (float2Double v))
  aFromJSON _ (JSON.Number (D v)) = Just (double2Float v)
  aFromJSON _ _ = Nothing

instance ADLValue T.Text where
  atype _ = "string"
  defaultv = T.empty
  aToJSON _ v = JSON.String v
  aFromJSON _ (JSON.String v) = Just v
  aFromJSON _ _ = Nothing

instance ADLValue B.ByteString where
  atype _ = "bytes"
  defaultv = B.empty

  aToJSON _ v = JSON.String (T.decodeUtf8 (B64.encode v))
  aFromJSON _ (JSON.String v) = case B64.decode (T.encodeUtf8 v) of
    (Left e) -> Nothing
    (Right v) -> Just v

instance forall a . (ADLValue a) => ADLValue [a] where
  atype _ = T.concat ["vector<",atype (undefined :: a),">"]

  defaultv = []

  aToJSON flags vs = JSON.Array (V.fromList (map (aToJSON flags) vs))

  aFromJSON flags (JSON.Array v) = mapM (aFromJSON flags) (V.toList v)
  aFromJSON _ _ = Nothing




