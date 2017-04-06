{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    S(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Sys.Types
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data S = S
    { s_f_pair :: (ADL.Sys.Types.Pair Data.Int.Int32 Prelude.Double)
    , s_f_either :: (ADL.Sys.Types.Either T.Text Data.Int.Int32)
    , s_f_error :: (ADL.Sys.Types.Error Data.Int.Int32)
    , s_f_map :: (ADL.Sys.Types.Map T.Text Prelude.Double)
    , s_f_set :: (ADL.Sys.Types.Set T.Text)
    , s_f_mstring :: (ADL.Sys.Types.Maybe T.Text)
    , s_f_mstring2 :: (ADL.Sys.Types.Maybe T.Text)
    , s_f_nstring :: (ADL.Sys.Types.Nullable T.Text)
    , s_f_nstring2 :: (ADL.Sys.Types.Nullable T.Text)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
        (Maybe_just "sukpeepolup")
        defaultv
        (Nullable_just "abcde")
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_pair_js = jsonSerialiser jf
            f_either_js = jsonSerialiser jf
            f_error_js = jsonSerialiser jf
            f_map_js = jsonSerialiser jf
            f_set_js = jsonSerialiser jf
            f_mstring_js = jsonSerialiser jf
            f_mstring2_js = jsonSerialiser jf
            f_nstring_js = jsonSerialiser jf
            f_nstring2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f_pair",aToJSON f_pair_js (s_f_pair v))
                , ("f_either",aToJSON f_either_js (s_f_either v))
                , ("f_error",aToJSON f_error_js (s_f_error v))
                , ("f_map",aToJSON f_map_js (s_f_map v))
                , ("f_set",aToJSON f_set_js (s_f_set v))
                , ("f_mstring",aToJSON f_mstring_js (s_f_mstring v))
                , ("f_mstring2",aToJSON f_mstring2_js (s_f_mstring2 v))
                , ("f_nstring",aToJSON f_nstring_js (s_f_nstring v))
                , ("f_nstring2",aToJSON f_nstring2_js (s_f_nstring2 v))
                ] )
            
            from (JSON.Object hm) = S 
                <$> fieldFromJSON f_pair_js "f_pair" defaultv hm
                <*> fieldFromJSON f_either_js "f_either" defaultv hm
                <*> fieldFromJSON f_error_js "f_error" defaultv hm
                <*> fieldFromJSON f_map_js "f_map" defaultv hm
                <*> fieldFromJSON f_set_js "f_set" defaultv hm
                <*> fieldFromJSON f_mstring_js "f_mstring" defaultv hm
                <*> fieldFromJSON f_mstring2_js "f_mstring2" defaultv hm
                <*> fieldFromJSON f_nstring_js "f_nstring" defaultv hm
                <*> fieldFromJSON f_nstring2_js "f_nstring2" defaultv hm
            from _ = Prelude.Nothing