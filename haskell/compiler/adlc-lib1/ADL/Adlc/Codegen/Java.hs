{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Codegen.Java(
    JavaParams(..),
    mkJavaParams,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Adlc.Codegen.Types
import qualified ADL.Core.Nullable
import qualified ADL.Sys.Adlast
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data JavaParams = JavaParams
    { javaParams_sources :: ADL.Adlc.Codegen.Types.AdlSources
    , javaParams_mergeExts :: [T.Text]
    , javaParams_modules :: [ADL.Sys.Adlast.ModuleName]
    , javaParams_package :: T.Text
    , javaParams_output :: ADL.Adlc.Codegen.Types.OutputParams
    , javaParams_generateTransitive :: Prelude.Bool
    , javaParams_includeRuntime :: Prelude.Bool
    , javaParams_parcellable :: Prelude.Bool
    , javaParams_hungarianNaming :: Prelude.Bool
    , javaParams_maxLineLength :: (ADL.Core.Nullable.Nullable Data.Word.Word16)
    , javaParams_headerComment :: T.Text
    , javaParams_suppressWarnings :: [T.Text]
    , javaParams_verbose :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Show)

mkJavaParams :: ADL.Adlc.Codegen.Types.AdlSources -> [ADL.Sys.Adlast.ModuleName] -> T.Text -> ADL.Adlc.Codegen.Types.OutputParams -> JavaParams
mkJavaParams sources modules package output = JavaParams sources [ ".adl-java" ] modules package output Prelude.True Prelude.True Prelude.False Prelude.False (ADL.Core.Nullable.null) "" [  ] Prelude.False

instance AdlValue JavaParams where
    atype _ = "adlc.codegen.java.JavaParams"
    
    jsonGen = genObject
        [ genField "sources" javaParams_sources
        , genField "mergeExts" javaParams_mergeExts
        , genField "modules" javaParams_modules
        , genField "package" javaParams_package
        , genField "output" javaParams_output
        , genField "generateTransitive" javaParams_generateTransitive
        , genField "includeRuntime" javaParams_includeRuntime
        , genField "parcellable" javaParams_parcellable
        , genField "hungarianNaming" javaParams_hungarianNaming
        , genField "maxLineLength" javaParams_maxLineLength
        , genField "headerComment" javaParams_headerComment
        , genField "suppressWarnings" javaParams_suppressWarnings
        , genField "verbose" javaParams_verbose
        ]
    
    jsonParser = JavaParams
        <$> parseField "sources"
        <*> parseFieldDef "mergeExts" [ ".adl-java" ]
        <*> parseField "modules"
        <*> parseField "package"
        <*> parseField "output"
        <*> parseFieldDef "generateTransitive" Prelude.True
        <*> parseFieldDef "includeRuntime" Prelude.True
        <*> parseFieldDef "parcellable" Prelude.False
        <*> parseFieldDef "hungarianNaming" Prelude.False
        <*> parseFieldDef "maxLineLength" (ADL.Core.Nullable.null)
        <*> parseFieldDef "headerComment" ""
        <*> parseFieldDef "suppressWarnings" [  ]
        <*> parseFieldDef "verbose" Prelude.False