{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Primitives where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Vector as V

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

instance ADLValue Int where
  atype _ = "int"

  defaultv = 0

  aToJSON _ v = JSON.Number (I (fromIntegral v))

  aFromJSON _ (JSON.Number (I v)) = Just (fromIntegral v)
  aFromJSON _ _ = Nothing

instance ADLValue Double where
  atype _ = "double"

  defaultv = 0

  aToJSON _ v = JSON.Number (D v)

  aFromJSON _ (JSON.Number (D v)) = Just v
  aFromJSON _ _ = Nothing

instance ADLValue T.Text where
  atype _ = "string"

  defaultv = T.empty

  aToJSON _ v = JSON.String v

  aFromJSON _ (JSON.String v) = Just v
  aFromJSON _ _ = Nothing

instance forall a . (ADLValue a) => ADLValue [a] where
  atype _ = T.concat ["vector<",atype (undefined :: a),">"]

  defaultv = []

  aToJSON flags vs = JSON.Array (V.fromList (map (aToJSON flags) vs))

  aFromJSON flags (JSON.Array v) = mapM (aFromJSON flags) (V.toList v)
  aFromJSON _ _ = Nothing




