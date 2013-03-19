{-# LANGUAGE OverloadedStrings #-}
module Primitive(
  PrimitiveType(..),
  ptFromText,
  ptToText,
  ptArgCount
  )where

import qualified Data.Map as Map
import qualified Data.Text as T

import Format

data PrimitiveType = P_Void
                   | P_Int
                   | P_Double
                   | P_Bool  
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
  , PrimitiveDetails P_Int "int" 0
  , PrimitiveDetails P_Double "double" 0
  , PrimitiveDetails P_ByteVector "bytes" 0
  , PrimitiveDetails P_String "string" 0
  , PrimitiveDetails P_Vector "vector" 1
  , PrimitiveDetails P_Sink "sink" 1
  ]

map1 = Map.fromList  [  (pd_type p, p) | p <- primitiveDetails ] 
map2 = Map.fromList  [  (pd_ident p, p) | p <- primitiveDetails ] 
    

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
  
