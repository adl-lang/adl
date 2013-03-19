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

  atoJSON _ () = JSON.Null

  afromJSON _ JSON.Null = Just ()
  afromJSON _ _ = Nothing

instance ADLValue Bool where
  atype _ = "bool"

  defaultv = False

  atoJSON _ v = JSON.Bool v

  afromJSON _ (JSON.Bool v) = Just v
  afromJSON _ _ = Nothing

instance ADLValue Int where
  atype _ = "int"

  defaultv = 0

  atoJSON _ v = JSON.Number (I (fromIntegral v))

  afromJSON _ (JSON.Number (I v)) = Just (fromIntegral v)
  afromJSON _ _ = Nothing

instance ADLValue Double where
  atype _ = "double"

  defaultv = 0

  atoJSON _ v = JSON.Number (D v)

  afromJSON _ (JSON.Number (D v)) = Just v
  afromJSON _ _ = Nothing

instance ADLValue T.Text where
  atype _ = "string"

  defaultv = T.empty

  atoJSON _ v = JSON.String v

  afromJSON _ (JSON.String v) = Just v
  afromJSON _ _ = Nothing

instance forall a . (ADLValue a) => ADLValue [a] where
  atype _ = T.concat ["vector<",atype (undefined :: a),">"]

  defaultv = []

  atoJSON flags vs = JSON.Array (V.fromList (map (atoJSON flags) vs))

  afromJSON flags (JSON.Array v) = mapM (afromJSON flags) (V.toList v)
  afromJSON _ _ = Nothing




