{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Haskell(
    HaskellCustomType(..),
    HaskellFieldPrefix,
    UnionConstructor(..),
    mkHaskellCustomType,
    mkUnionConstructor,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data HaskellCustomType = HaskellCustomType
    { haskellCustomType_haskellname :: T.Text
    , haskellCustomType_haskellimports :: [T.Text]
    , haskellCustomType_haskellextraexports :: [T.Text]
    , haskellCustomType_insertCode :: [T.Text]
    , haskellCustomType_generateOrigADLType :: T.Text
    , haskellCustomType_structConstructor :: T.Text
    , haskellCustomType_unionConstructors :: [UnionConstructor]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkHaskellCustomType :: T.Text -> [T.Text] -> [T.Text] -> HaskellCustomType
mkHaskellCustomType haskellname haskellimports insertCode = HaskellCustomType haskellname haskellimports [  ] insertCode "" "" [  ]

instance AdlValue HaskellCustomType where
    atype _ = "adlc.config.haskell.HaskellCustomType"
    
    jsonGen = genObject
        [ genField "haskellname" haskellCustomType_haskellname
        , genField "haskellimports" haskellCustomType_haskellimports
        , genField "haskellextraexports" haskellCustomType_haskellextraexports
        , genField "insertCode" haskellCustomType_insertCode
        , genField "generateOrigADLType" haskellCustomType_generateOrigADLType
        , genField "structConstructor" haskellCustomType_structConstructor
        , genField "unionConstructors" haskellCustomType_unionConstructors
        ]
    
    jsonParser = HaskellCustomType
        <$> parseField "haskellname"
        <*> parseField "haskellimports"
        <*> parseFieldDef "haskellextraexports" [  ]
        <*> parseField "insertCode"
        <*> parseFieldDef "generateOrigADLType" ""
        <*> parseFieldDef "structConstructor" ""
        <*> parseFieldDef "unionConstructors" [  ]

type HaskellFieldPrefix = T.Text

data UnionConstructor = UnionConstructor
    { unionConstructor_fieldName :: T.Text
    , unionConstructor_constructor :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkUnionConstructor :: T.Text -> T.Text -> UnionConstructor
mkUnionConstructor fieldName constructor = UnionConstructor fieldName constructor

instance AdlValue UnionConstructor where
    atype _ = "adlc.config.haskell.UnionConstructor"
    
    jsonGen = genObject
        [ genField "fieldName" unionConstructor_fieldName
        , genField "constructor" unionConstructor_constructor
        ]
    
    jsonParser = UnionConstructor
        <$> parseField "fieldName"
        <*> parseField "constructor"