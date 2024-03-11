{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Dynamic(
    Dynamic(..),
    mkDynamic,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Sys.Adlast
import qualified Data.Aeson as JS
import qualified Data.Proxy
import qualified Prelude

data Dynamic = Dynamic
    { dynamic_typeExpr :: ADL.Sys.Adlast.TypeExpr
    , dynamic_value :: JS.Value
    }
    deriving (Prelude.Eq,Prelude.Show)

mkDynamic :: ADL.Sys.Adlast.TypeExpr -> JS.Value -> Dynamic
mkDynamic typeExpr value = Dynamic typeExpr value

instance AdlValue Dynamic where
    atype _ = "sys.dynamic.Dynamic"
    
    jsonGen = genObject
        [ genField "typeExpr" dynamic_typeExpr
        , genField "value" dynamic_value
        ]
    
    jsonParser = Dynamic
        <$> parseField "typeExpr"
        <*> parseField "value"