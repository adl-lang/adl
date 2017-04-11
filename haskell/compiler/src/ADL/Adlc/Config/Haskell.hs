{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Haskell(
    HaskellCustomType(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data HaskellCustomType = HaskellCustomType
    { haskellCustomType_haskellname :: T.Text
    , haskellCustomType_haskellimports :: [T.Text]
    , haskellCustomType_insertCode :: [T.Text]
    , haskellCustomType_generateOrigADLType :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue HaskellCustomType where
    atype _ = "adlc.config.haskell.HaskellCustomType"
    
    defaultv = HaskellCustomType
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "haskellname" haskellCustomType_haskellname
        , genField "haskellimports" haskellCustomType_haskellimports
        , genField "insertCode" haskellCustomType_insertCode
        , genField "generateOrigADLType" haskellCustomType_generateOrigADLType
        ]
    
    jsonParser = HaskellCustomType
        <$> parseField "haskellname"
        <*> parseField "haskellimports"
        <*> parseField "insertCode"
        <*> parseField "generateOrigADLType"