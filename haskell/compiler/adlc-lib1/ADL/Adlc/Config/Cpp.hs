{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Cpp(
    CppCustomType(..),
    Include(..),
    mkCppCustomType,
    mkInclude,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data CppCustomType = CppCustomType
    { cppCustomType_cppname :: T.Text
    , cppCustomType_cppincludes :: [Include]
    , cppCustomType_declarationCode :: [T.Text]
    , cppCustomType_serialisationCode :: [T.Text]
    , cppCustomType_generateOrigADLType :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCppCustomType :: T.Text -> CppCustomType
mkCppCustomType cppname = CppCustomType cppname [  ] [  ] [  ] ""

instance AdlValue CppCustomType where
    atype _ = "adlc.config.cpp.CppCustomType"
    
    jsonGen = genObject
        [ genField "cppname" cppCustomType_cppname
        , genField "cppincludes" cppCustomType_cppincludes
        , genField "declarationCode" cppCustomType_declarationCode
        , genField "serialisationCode" cppCustomType_serialisationCode
        , genField "generateOrigADLType" cppCustomType_generateOrigADLType
        ]
    
    jsonParser = CppCustomType
        <$> parseField "cppname"
        <*> parseFieldDef "cppincludes" [  ]
        <*> parseFieldDef "declarationCode" [  ]
        <*> parseFieldDef "serialisationCode" [  ]
        <*> parseFieldDef "generateOrigADLType" ""

data Include = Include
    { include_name :: T.Text
    , include_system :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkInclude :: T.Text -> Prelude.Bool -> Include
mkInclude name system = Include name system

instance AdlValue Include where
    atype _ = "adlc.config.cpp.Include"
    
    jsonGen = genObject
        [ genField "name" include_name
        , genField "system" include_system
        ]
    
    jsonParser = Include
        <$> parseField "name"
        <*> parseField "system"