{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Cpp(
    CppCustomType(..),
    Include(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
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

instance AdlValue CppCustomType where
    atype _ = "adlc.config.cpp.CppCustomType"
    
    defaultv = CppCustomType
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "cppname" cppCustomType_cppname
        , genField "cppincludes" cppCustomType_cppincludes
        , genField "declarationCode" cppCustomType_declarationCode
        , genField "serialisationCode" cppCustomType_serialisationCode
        , genField "generateOrigADLType" cppCustomType_generateOrigADLType
        ]
    
    jsonParser = CppCustomType
        <$> parseField "cppname"
        <*> parseField "cppincludes"
        <*> parseField "declarationCode"
        <*> parseField "serialisationCode"
        <*> parseField "generateOrigADLType"

data Include = Include
    { include_name :: T.Text
    , include_system :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Include where
    atype _ = "adlc.config.cpp.Include"
    
    defaultv = Include
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "name" include_name
        , genField "system" include_system
        ]
    
    jsonParser = Include
        <$> parseField "name"
        <*> parseField "system"