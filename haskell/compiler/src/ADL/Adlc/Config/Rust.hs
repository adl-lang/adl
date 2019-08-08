{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Rust(
    RustCustomType(..),
    RustGenerate,
    RustStorageModel(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data RustCustomType = RustCustomType
    { rustCustomType_rustname :: T.Text
    , rustCustomType_helpers :: T.Text
    , rustCustomType_generateOrigADLType :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkRustCustomType :: T.Text -> T.Text -> RustCustomType
mkRustCustomType rustname helpers = RustCustomType rustname helpers ""

instance AdlValue RustCustomType where
    atype _ = "adlc.config.rust.RustCustomType"
    
    jsonGen = genObject
        [ genField "rustname" rustCustomType_rustname
        , genField "helpers" rustCustomType_helpers
        , genField "generateOrigADLType" rustCustomType_generateOrigADLType
        ]
    
    jsonParser = RustCustomType
        <$> parseField "rustname"
        <*> parseField "helpers"
        <*> parseFieldDef "generateOrigADLType" ""

type RustGenerate = Prelude.Bool

data RustStorageModel
    = RustStorageModel_standard
    | RustStorageModel_boxed
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue RustStorageModel where
    atype _ = "adlc.config.rust.RustStorageModel"
    
    jsonGen = genUnion (\jv -> case jv of
        RustStorageModel_standard -> genUnionVoid "standard"
        RustStorageModel_boxed -> genUnionVoid "boxed"
        )
    
    jsonParser
        =   parseUnionVoid "standard" RustStorageModel_standard
        <|> parseUnionVoid "boxed" RustStorageModel_boxed
        <|> parseFail "expected a RustStorageModel"