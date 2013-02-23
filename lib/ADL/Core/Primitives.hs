module ADL.Core.Primitives where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Attoparsec.Number

import ADL.Core.Value

instance ADLValue () where
  defaultv = ()

  atoJSON _ () = JSON.Null

  afromJSON _ JSON.Null = Just ()
  afromJSON _ _ = Nothing

instance ADLValue Int where
  defaultv = 0

  atoJSON _ v = JSON.Number (I (fromIntegral v))

  afromJSON _ (JSON.Number (I v)) = Just (fromIntegral v)
  afromJSON _ _ = Nothing

instance ADLValue Double where
  defaultv = 0

  atoJSON _ v = JSON.Number (D v)

  afromJSON _ (JSON.Number (D v)) = Just v
  afromJSON _ _ = Nothing

instance ADLValue T.Text where
  defaultv = T.empty

  atoJSON _ v = JSON.String v

  afromJSON _ (JSON.String v) = Just v
  afromJSON _ _ = Nothing

instance (ADLValue a) => ADLValue [a] where
  defaultv = []

  atoJSON flags vs = JSON.Array (V.fromList (map (atoJSON flags) vs))

  afromJSON flags (JSON.Array v) = mapM (afromJSON flags) (V.toList v)
  afromJSON _ _ = Nothing




