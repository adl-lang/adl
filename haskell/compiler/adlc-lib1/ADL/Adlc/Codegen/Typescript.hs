{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Codegen.Typescript(
    TypescriptParams(..),
    TypescriptStyle(..),
    mkTypescriptParams,
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

data TypescriptParams = TypescriptParams
    { typescriptParams_sources :: ADL.Adlc.Codegen.Types.AdlSources
    , typescriptParams_mergeExts :: [T.Text]
    , typescriptParams_modules :: [ADL.Sys.Adlast.ModuleName]
    , typescriptParams_style :: TypescriptStyle
    , typescriptParams_output :: ADL.Adlc.Codegen.Types.OutputParams
    , typescriptParams_runtimePath :: T.Text
    , typescriptParams_generateTransitive :: Prelude.Bool
    , typescriptParams_includeRuntime :: Prelude.Bool
    , typescriptParams_includeResolver :: Prelude.Bool
    , typescriptParams_excludeAst :: Prelude.Bool
    , typescriptParams_excludedAstAnnotations :: [ADL.Sys.Adlast.ScopedName]
    , typescriptParams_verbose :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Show)

mkTypescriptParams :: ADL.Adlc.Codegen.Types.AdlSources -> [ADL.Sys.Adlast.ModuleName] -> TypescriptStyle -> ADL.Adlc.Codegen.Types.OutputParams -> TypescriptParams
mkTypescriptParams sources modules style output = TypescriptParams sources [ ".adl-ts" ] modules style output "runtime" Prelude.True Prelude.True Prelude.True Prelude.False [ (ADL.Sys.Adlast.ScopedName "sys.annotations" "Doc") ] Prelude.False

instance AdlValue TypescriptParams where
    atype _ = "adlc.codegen.typescript.TypescriptParams"
    
    jsonGen = genObject
        [ genField "sources" typescriptParams_sources
        , genField "mergeExts" typescriptParams_mergeExts
        , genField "modules" typescriptParams_modules
        , genField "style" typescriptParams_style
        , genField "output" typescriptParams_output
        , genField "runtimePath" typescriptParams_runtimePath
        , genField "generateTransitive" typescriptParams_generateTransitive
        , genField "includeRuntime" typescriptParams_includeRuntime
        , genField "includeResolver" typescriptParams_includeResolver
        , genField "excludeAst" typescriptParams_excludeAst
        , genField "excludedAstAnnotations" typescriptParams_excludedAstAnnotations
        , genField "verbose" typescriptParams_verbose
        ]
    
    jsonParser = TypescriptParams
        <$> parseField "sources"
        <*> parseFieldDef "mergeExts" [ ".adl-ts" ]
        <*> parseField "modules"
        <*> parseField "style"
        <*> parseField "output"
        <*> parseFieldDef "runtimePath" "runtime"
        <*> parseFieldDef "generateTransitive" Prelude.True
        <*> parseFieldDef "includeRuntime" Prelude.True
        <*> parseFieldDef "includeResolver" Prelude.True
        <*> parseFieldDef "excludeAst" Prelude.False
        <*> parseFieldDef "excludedAstAnnotations" [ (ADL.Sys.Adlast.ScopedName "sys.annotations" "Doc") ]
        <*> parseFieldDef "verbose" Prelude.False

data TypescriptStyle
    = TypescriptStyle_tsc
    | TypescriptStyle_deno
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue TypescriptStyle where
    atype _ = "adlc.codegen.typescript.TypescriptStyle"
    
    jsonGen = genUnion (\jv -> case jv of
        TypescriptStyle_tsc -> genUnionVoid "tsc"
        TypescriptStyle_deno -> genUnionVoid "deno"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "tsc" -> parseUnionVoid TypescriptStyle_tsc
        "deno" -> parseUnionVoid TypescriptStyle_deno
        _ -> parseFail "expected a discriminator for TypescriptStyle (tsc,deno)" 