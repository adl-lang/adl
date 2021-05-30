{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Codegen.Types(
    AdlSources,
    AdlTreeSource(..),
    FilePath,
    OutputParams(..),
    mkOutputParams,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Nullable
import qualified ADL.Sys.Adlast
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type AdlSources = [AdlTreeSource]

data AdlTreeSource
    = AdlTreeSource_localDir FilePath
    | AdlTreeSource_modules (StringMap ADL.Sys.Adlast.Module)
    deriving (Prelude.Eq,Prelude.Show)

instance AdlValue AdlTreeSource where
    atype _ = "adlc.codegen.types.AdlTreeSource"
    
    jsonGen = genUnion (\jv -> case jv of
        AdlTreeSource_localDir v -> genUnionValue "localDir" v
        AdlTreeSource_modules v -> genUnionValue "modules" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "localDir" ->  parseUnionValue AdlTreeSource_localDir
        "modules" ->  parseUnionValue AdlTreeSource_modules
        _ -> parseFail "expected a discriminator for AdlTreeSource (localDir,modules)" 

type FilePath = T.Text

data OutputParams = OutputParams
    { outputParams_outputDir :: FilePath
    , outputParams_noOverwrite :: Prelude.Bool
    , outputParams_manifest :: (ADL.Core.Nullable.Nullable FilePath)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkOutputParams :: FilePath -> OutputParams
mkOutputParams outputDir = OutputParams outputDir Prelude.True (ADL.Core.Nullable.fromValue (".adl-manifest"))

instance AdlValue OutputParams where
    atype _ = "adlc.codegen.types.OutputParams"
    
    jsonGen = genObject
        [ genField "outputDir" outputParams_outputDir
        , genField "noOverwrite" outputParams_noOverwrite
        , genField "manifest" outputParams_manifest
        ]
    
    jsonParser = OutputParams
        <$> parseField "outputDir"
        <*> parseFieldDef "noOverwrite" Prelude.True
        <*> parseFieldDef "manifest" (ADL.Core.Nullable.fromValue (".adl-manifest"))