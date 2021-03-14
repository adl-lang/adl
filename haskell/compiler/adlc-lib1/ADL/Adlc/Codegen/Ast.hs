{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Codegen.Ast(
    AstParams(..),
    mkAstParams,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Adlc.Codegen.Types
import qualified ADL.Sys.Adlast
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data AstParams = AstParams
    { astParams_sources :: ADL.Adlc.Codegen.Types.AdlSources
    , astParams_mergeExts :: [T.Text]
    , astParams_modules :: [ADL.Sys.Adlast.ModuleName]
    , astParams_generateTransitive :: Prelude.Bool
    , astParams_outputFile :: ADL.Adlc.Codegen.Types.FilePath
    }
    deriving (Prelude.Eq,Prelude.Show)

mkAstParams :: ADL.Adlc.Codegen.Types.AdlSources -> [ADL.Sys.Adlast.ModuleName] -> ADL.Adlc.Codegen.Types.FilePath -> AstParams
mkAstParams sources modules outputFile = AstParams sources [  ] modules Prelude.True outputFile

instance AdlValue AstParams where
    atype _ = "adlc.codegen.ast.AstParams"
    
    jsonGen = genObject
        [ genField "sources" astParams_sources
        , genField "mergeExts" astParams_mergeExts
        , genField "modules" astParams_modules
        , genField "generateTransitive" astParams_generateTransitive
        , genField "outputFile" astParams_outputFile
        ]
    
    jsonParser = AstParams
        <$> parseField "sources"
        <*> parseFieldDef "mergeExts" [  ]
        <*> parseField "modules"
        <*> parseFieldDef "generateTransitive" Prelude.True
        <*> parseField "outputFile"