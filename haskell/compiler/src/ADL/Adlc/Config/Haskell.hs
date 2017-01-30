{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Haskell(
    HaskellCustomType(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

data HaskellCustomType = HaskellCustomType
    { haskellCustomType_haskellname :: T.Text
    , haskellCustomType_haskellimports :: [T.Text]
    , haskellCustomType_insertCode :: [T.Text]
    , haskellCustomType_generateOrigADLType :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue HaskellCustomType where
    atype _ = "adlc.config.haskell.HaskellCustomType"
    
    defaultv = HaskellCustomType
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            haskellname_js = jsonSerialiser jf
            haskellimports_js = jsonSerialiser jf
            insertCode_js = jsonSerialiser jf
            generateOrigADLType_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("haskellname",aToJSON haskellname_js (haskellCustomType_haskellname v))
                , ("haskellimports",aToJSON haskellimports_js (haskellCustomType_haskellimports v))
                , ("insertCode",aToJSON insertCode_js (haskellCustomType_insertCode v))
                , ("generateOrigADLType",aToJSON generateOrigADLType_js (haskellCustomType_generateOrigADLType v))
                ] )
            
            from (JSON.Object hm) = HaskellCustomType 
                <$> fieldFromJSON haskellname_js "haskellname" defaultv hm
                <*> fieldFromJSON haskellimports_js "haskellimports" defaultv hm
                <*> fieldFromJSON insertCode_js "insertCode" defaultv hm
                <*> fieldFromJSON generateOrigADLType_js "generateOrigADLType" defaultv hm
            from _ = Prelude.Nothing