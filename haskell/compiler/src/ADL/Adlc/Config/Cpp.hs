{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Cpp(
    CppCustomType(..),
    Include(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
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

instance ADLValue CppCustomType where
    atype _ = "adlc.config.cpp.CppCustomType"
    
    defaultv = CppCustomType
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            cppname_js = jsonSerialiser jf
            cppincludes_js = jsonSerialiser jf
            declarationCode_js = jsonSerialiser jf
            serialisationCode_js = jsonSerialiser jf
            generateOrigADLType_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("cppname",aToJSON cppname_js (cppCustomType_cppname v))
                , ("cppincludes",aToJSON cppincludes_js (cppCustomType_cppincludes v))
                , ("declarationCode",aToJSON declarationCode_js (cppCustomType_declarationCode v))
                , ("serialisationCode",aToJSON serialisationCode_js (cppCustomType_serialisationCode v))
                , ("generateOrigADLType",aToJSON generateOrigADLType_js (cppCustomType_generateOrigADLType v))
                ] )
            
            from (JSON.Object hm) = CppCustomType 
                <$> fieldFromJSON cppname_js "cppname" defaultv hm
                <*> fieldFromJSON cppincludes_js "cppincludes" defaultv hm
                <*> fieldFromJSON declarationCode_js "declarationCode" defaultv hm
                <*> fieldFromJSON serialisationCode_js "serialisationCode" defaultv hm
                <*> fieldFromJSON generateOrigADLType_js "generateOrigADLType" defaultv hm
            from _ = Prelude.Nothing

data Include = Include
    { include_name :: T.Text
    , include_system :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Include where
    atype _ = "adlc.config.cpp.Include"
    
    defaultv = Include
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            name_js = jsonSerialiser jf
            system_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("name",aToJSON name_js (include_name v))
                , ("system",aToJSON system_js (include_system v))
                ] )
            
            from (JSON.Object hm) = Include 
                <$> fieldFromJSON name_js "name" defaultv hm
                <*> fieldFromJSON system_js "system" defaultv hm
            from _ = Prelude.Nothing