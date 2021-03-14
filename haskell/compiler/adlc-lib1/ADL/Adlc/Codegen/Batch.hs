{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Codegen.Batch(
    Batch,
    BatchItem(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Adlc.Codegen.Ast
import qualified ADL.Adlc.Codegen.Java
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Prelude

type Batch = [BatchItem]

data BatchItem
    = BatchItem_ast ADL.Adlc.Codegen.Ast.AstParams
    | BatchItem_java ADL.Adlc.Codegen.Java.JavaParams
    deriving (Prelude.Eq,Prelude.Show)

instance AdlValue BatchItem where
    atype _ = "adlc.codegen.batch.BatchItem"
    
    jsonGen = genUnion (\jv -> case jv of
        BatchItem_ast v -> genUnionValue "ast" v
        BatchItem_java v -> genUnionValue "java" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "ast" ->  parseUnionValue BatchItem_ast
        "java" ->  parseUnionValue BatchItem_java
        _ -> parseFail "expected a discriminator for BatchItem (ast,java)" 