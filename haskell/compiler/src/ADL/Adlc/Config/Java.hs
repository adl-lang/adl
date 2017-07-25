{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Java(
    JavaCustomType(..),
    JavaPackage,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data JavaCustomType = JavaCustomType
    { javaCustomType_javaname :: T.Text
    , javaCustomType_helpers :: T.Text
    , javaCustomType_generateType :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkJavaCustomType :: T.Text -> T.Text -> JavaCustomType
mkJavaCustomType javaname helpers = JavaCustomType javaname helpers Prelude.False

instance AdlValue JavaCustomType where
    atype _ = "adlc.config.java.JavaCustomType"
    
    jsonGen = genObject
        [ genField "javaname" javaCustomType_javaname
        , genField "helpers" javaCustomType_helpers
        , genField "generateType" javaCustomType_generateType
        ]
    
    jsonParser = JavaCustomType
        <$> parseField "javaname"
        <*> parseField "helpers"
        <*> parseFieldDef "generateType" Prelude.False

type JavaPackage = T.Text