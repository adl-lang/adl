{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Java(
    JavaCustomType(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

data JavaCustomType = JavaCustomType
    { javaCustomType_javaname :: T.Text
    , javaCustomType_helpers :: T.Text
    , javaCustomType_generateType :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue JavaCustomType where
    atype _ = "adlc.config.java.JavaCustomType"
    
    defaultv = JavaCustomType
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            javaname_js = jsonSerialiser jf
            helpers_js = jsonSerialiser jf
            generateType_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("javaname",aToJSON javaname_js (javaCustomType_javaname v))
                , ("helpers",aToJSON helpers_js (javaCustomType_helpers v))
                , ("generateType",aToJSON generateType_js (javaCustomType_generateType v))
                ] )
            
            from (JSON.Object hm) = JavaCustomType 
                <$> fieldFromJSON javaname_js "javaname" defaultv hm
                <*> fieldFromJSON helpers_js "helpers" defaultv hm
                <*> fieldFromJSON generateType_js "generateType" defaultv hm
            from _ = Prelude.Nothing