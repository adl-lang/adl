{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.TypeToken(
  TypeToken(..)
  ) where

import qualified Data.Aeson as JS
import qualified Data.Text as T
import Data.Proxy
import ADL.Core.Value

data TypeToken a = TypeToken
 deriving (Eq,Ord,Show)

instance AdlValue a => AdlValue (TypeToken a) where
  atype _ = T.concat ["TypeToken<",atype (Proxy :: Proxy a),">"]

  jsonGen = JsonGen (const JS.Null)

  jsonParser = JsonParser $ \ctx v -> case v of
    JS.Null -> pure TypeToken
    _ -> ParseFailure "expected null" ctx
