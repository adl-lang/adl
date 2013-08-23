{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Primitive(
  PrimitiveType(..),
  ptFromText,
  ptToText,
  ptArgCount,
  ptValidateLiteral
  )where

import Data.Int
import Data.Word
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Aeson as JSON
import Data.Attoparsec.Number as JSON
import qualified Data.ByteString.Base64 as B64

import ADL.Utils.Format

data PrimitiveType = P_Void
                   | P_Bool  
                   | P_Int8
                   | P_Int16
                   | P_Int32
                   | P_Int64
                   | P_Word8
                   | P_Word16
                   | P_Word32
                   | P_Word64
                   | P_Float
                   | P_Double
                   | P_ByteVector
                   | P_String
                   | P_Vector
                   | P_Sink
  deriving (Eq,Ord,Show)

instance Format PrimitiveType where
  formatText = ptToText

data PrimitiveDetails = PrimitiveDetails {
   pd_type :: PrimitiveType,
   pd_ident :: T.Text,
   pd_nArgs :: Int,
   pd_validateLiteral :: JSON.Value -> Bool
}

primitiveDetails =
  [ PrimitiveDetails P_Void "Void" 0 isVoid
  , PrimitiveDetails P_Bool "Bool" 0 isBool
  , PrimitiveDetails P_Int8 "Int8" 0 (isInt (minBound::Int8) (maxBound::Int8))
  , PrimitiveDetails P_Int16 "Int16" 0 (isInt (minBound::Int16) (maxBound::Int16))
  , PrimitiveDetails P_Int32 "Int32" 0 (isInt (minBound::Int32) (maxBound::Int32))
  , PrimitiveDetails P_Int64 "Int64" 0 (isInt (minBound::Int64) (maxBound::Int64))
  , PrimitiveDetails P_Word8 "Word8" 0 (isWord (maxBound::Word8))
  , PrimitiveDetails P_Word16 "Word16" 0 (isWord (maxBound::Word16))
  , PrimitiveDetails P_Word32 "Word32" 0 (isWord (maxBound::Word32))
  , PrimitiveDetails P_Word64 "Word64" 0 (isWord (maxBound::Word64))
  , PrimitiveDetails P_Float "Float" 0 isFloat
  , PrimitiveDetails P_Double "Double" 0 isFloat
  , PrimitiveDetails P_ByteVector "Bytes" 0 isBytes
  , PrimitiveDetails P_String "String" 0 isString
  , PrimitiveDetails P_Vector "Vector" 1 (const False)
  , PrimitiveDetails P_Sink "Sink" 1 (const False)
  ]

isVoid JSON.Null = True
isVoid _ = False

isBool (JSON.Bool _) = True
isBool _ = False

isInt :: Integral a => a -> a -> JSON.Value -> Bool
isInt min max (JSON.Number (JSON.I n)) = n >= fromIntegral min && n <= fromIntegral max
isInt _ _ _  = False

isWord :: Integral a => a -> JSON.Value -> Bool
isWord max (JSON.Number (JSON.I n)) = n >= 0 && n <= fromIntegral max
isWord _ _  = False

isFloat (JSON.Number (JSON.I _)) = True
isFloat (JSON.Number (JSON.D _)) = True
isFloat _ = False

isString (JSON.String _) = True
isString _ = False

isBytes (JSON.String s) = case B64.decode (T.encodeUtf8 s) of
  (Left _) -> False
  (Right _) -> True
isBytes _ = False

map1 = Map.fromList  [  (pd_type p, p) | p <- primitiveDetails ] 
map2 = Map.fromList [ (pd_ident p, p) | p <- primitiveDetails ]
    

ptFromText :: T.Text -> Maybe PrimitiveType
ptFromText i = case Map.lookup i map2 of
  (Just p) -> Just (pd_type p)
  Nothing -> Nothing

ptToText :: PrimitiveType -> T.Text
ptToText p = pd_ident (plookup p)

ptArgCount :: PrimitiveType -> Int
ptArgCount p = pd_nArgs (plookup p)

ptValidateLiteral :: PrimitiveType -> JSON.Value -> Maybe T.Text
ptValidateLiteral p v =
  if pd_validateLiteral pd v
  then Nothing
  else (Just (T.concat ["expected ",pd_ident pd]))
  where
    pd = plookup p

plookup p = case Map.lookup p map1 of
  (Just pd) -> pd
  Nothing -> error ("Missing details for primitive type " ++ show p)
  
