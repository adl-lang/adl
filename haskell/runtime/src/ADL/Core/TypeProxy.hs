{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.TypeToken(
  TypeToken(..),
  typeProxy
) where

import qualified Data.Aeson as JS
import qualified Data.Text as T

import ADL.Core.Value
import Data.Proxy

-- Only a placeholder for now
data TypeToken a = TypeToken
  deriving (Eq,Ord,Show)

typeProxy = TypeToken

instance (AdlValue t) => AdlValue (TypeToken t) where
  atype _ = T.concat
    [ "TypeToken"
    , "<", atype (Proxy :: Proxy t)
    , ">" ]

  jsonGen = JsonGen (\v -> case v of
      _ -> JS.Null
    )
  jsonParser = JsonParser (\ctx jv -> case jv of
      JS.Null -> pure TypeToken
      _       -> ParseFailure "expected null for type token" ctx
    )
