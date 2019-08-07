{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Rust(
    RustGenerate,
    RustStorageModel(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Prelude

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