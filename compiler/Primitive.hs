{-# LANGUAGE OverloadedStrings #-}
module Primitive(
  PrimitiveType(..),
  ptFromText,
  ptToText,
  ptArgCount
  )where

import qualified Data.Map as Map
import qualified Data.Text as T

import ADL.Utils.Format

data PrimitiveType = P_Void
                   | P_Bool  
                   | P_Int8
                   | P_Int16
                   | P_Int32
                   | P_Int64
                   | P_UInt8
                   | P_UInt16
                   | P_UInt32
                   | P_UInt64
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
   pd_nArgs :: Int
}

primitiveDetails =
  [ PrimitiveDetails P_Void "void" 0
  , PrimitiveDetails P_Bool "bool" 0
  , PrimitiveDetails P_Int8 "int8" 0
  , PrimitiveDetails P_Int16 "int16" 0
  , PrimitiveDetails P_Int32 "int32" 0
  , PrimitiveDetails P_Int64 "int64" 0
  , PrimitiveDetails P_UInt8 "uint8" 0
  , PrimitiveDetails P_UInt16 "uint16" 0
  , PrimitiveDetails P_UInt32 "uint32" 0
  , PrimitiveDetails P_UInt64 "uint64" 0
  , PrimitiveDetails P_Float "float" 0
  , PrimitiveDetails P_Double "double" 0
  , PrimitiveDetails P_ByteVector "bytes" 0
  , PrimitiveDetails P_String "string" 0
  , PrimitiveDetails P_Vector "vector" 1
  , PrimitiveDetails P_Sink "sink" 1
  ]

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

plookup p = case Map.lookup p map1 of
  (Just pd) -> pd
  Nothing -> error ("Missing details for primitive type " ++ show p)
  
