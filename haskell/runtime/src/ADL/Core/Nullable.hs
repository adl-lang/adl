{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Nullable(
  Nullable(..),
  null,
  fromValue,
  fromMaybe,
) where

import Prelude hiding (null)
import qualified Data.Aeson as JS
import qualified Data.Text as T

import ADL.Core.Value
import Data.Proxy


-- | Nullable is isomorphic to Maybe, but must be distinct
newtype Nullable a =  Nullable {unNullable :: Maybe a}
  deriving (Eq,Ord,Show)

null :: Nullable a
null = Nullable Nothing

fromValue :: a -> Nullable a
fromValue a = Nullable (Just a)

fromMaybe ::  Maybe a-> Nullable a
fromMaybe = Nullable

instance (AdlValue t) => AdlValue (Nullable t) where
  atype _ = T.concat
    [ "Nullable"
    , "<", atype (Proxy :: Proxy t)
    , ">" ]

  jsonGen = JsonGen (\v -> case v of
      (Nullable Nothing) -> JS.Null
      (Nullable (Just v1)) -> adlToJson v1
    )
  jsonParser = JsonParser (\ctx jv -> case jv of
      JS.Null -> (ParseSuccess (Nullable Nothing))
      _       -> fmap (Nullable . Just) (runJsonParser jsonParser ctx jv)
    )
