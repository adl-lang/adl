{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test(
    A(..),
    B(..),
    S(..),
    U(..),
    XY(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data A = A
    { a_f_int :: Data.Int.Int16
    , a_f_string :: T.Text
    , a_f_bool :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkA v_f_int v_f_string v_f_bool = A v_f_int v_f_string v_f_bool

instance ADLValue A where
    atype _ = "test.A"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_int_js = jsonSerialiser jf
            f_string_js = jsonSerialiser jf
            f_bool_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f_int",aToJSON f_int_js (a_f_int v))
                , ("f_string",aToJSON f_string_js (a_f_string v))
                , ("f_bool",aToJSON f_bool_js (a_f_bool v))
                ] )
            
            from (JSON.Object hm) = A 
                <$> fieldFromJSON f_int_js "f_int" hm
                <*> fieldFromJSON f_string_js "f_string" hm
                <*> fieldFromJSON f_bool_js "f_bool" hm
            from _ = Prelude.Nothing

data B t = B
    { b_f_t :: t
    , b_f_string :: T.Text
    , b_f_tvec :: [t]
    , b_f_xy :: (XY t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkB v_f_t v_f_string v_f_tvec v_f_xy = B v_f_t v_f_string v_f_tvec v_f_xy

instance (ADLValue t) => ADLValue (B t) where
    atype _ = T.concat
        [ "test.B"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_t_js = jsonSerialiser jf
            f_string_js = jsonSerialiser jf
            f_tvec_js = jsonSerialiser jf
            f_xy_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f_t",aToJSON f_t_js (b_f_t v))
                , ("f_string",aToJSON f_string_js (b_f_string v))
                , ("f_tvec",aToJSON f_tvec_js (b_f_tvec v))
                , ("f_xy",aToJSON f_xy_js (b_f_xy v))
                ] )
            
            from (JSON.Object hm) = B 
                <$> fieldFromJSON f_t_js "f_t" hm
                <*> fieldFromJSON f_string_js "f_string" hm
                <*> fieldFromJSON f_tvec_js "f_tvec" hm
                <*> fieldFromJSON f_xy_js "f_xy" hm
            from _ = Prelude.Nothing

data S t = S
    { s_f_void :: ()
    , s_f_bool :: Prelude.Bool
    , s_f_int8 :: Data.Int.Int8
    , s_f_int16 :: Data.Int.Int16
    , s_f_int32 :: Data.Int.Int32
    , s_f_int64 :: Data.Int.Int64
    , s_f_word8 :: Data.Word.Word8
    , s_f_word16 :: Data.Word.Word16
    , s_f_word32 :: Data.Word.Word32
    , s_f_word64 :: Data.Word.Word64
    , s_f_float :: Prelude.Float
    , s_f_double :: Prelude.Double
    , s_f_bytes :: B.ByteString
    , s_f_string :: T.Text
    , s_f_vstring :: [T.Text]
    , s_f_a :: A
    , s_f_u :: U
    , s_f_t :: t
    , s_f_bint16 :: (B Data.Int.Int16)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS v_f_t = S () Prelude.True (-5) (-10000) 56 40000 32 50000 124456 2344 0.5 0.45 "hello" "abcd" [ "xy", "ab" ] A{ a_f_bool = Prelude.True, a_f_int = 31, a_f_string = "xyz" } (U_f_int 45) v_f_t B{ b_f_string = "yikes", b_f_t = 56, b_f_tvec = [ 1, 2, 3 ], b_f_xy = XY{ xY_x = 5, xY_y = 5 } }

instance (ADLValue t) => ADLValue (S t) where
    atype _ = T.concat
        [ "test.S"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_void_js = jsonSerialiser jf
            f_bool_js = jsonSerialiser jf
            f_int8_js = jsonSerialiser jf
            f_int16_js = jsonSerialiser jf
            f_int32_js = jsonSerialiser jf
            f_int64_js = jsonSerialiser jf
            f_word8_js = jsonSerialiser jf
            f_word16_js = jsonSerialiser jf
            f_word32_js = jsonSerialiser jf
            f_word64_js = jsonSerialiser jf
            f_float_js = jsonSerialiser jf
            f_double_js = jsonSerialiser jf
            f_bytes_js = jsonSerialiser jf
            f_string_js = jsonSerialiser jf
            f_vstring_js = jsonSerialiser jf
            f_a_js = jsonSerialiser jf
            f_u_js = jsonSerialiser jf
            f_t_js = jsonSerialiser jf
            f_bint16_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f_void",aToJSON f_void_js (s_f_void v))
                , ("f_bool",aToJSON f_bool_js (s_f_bool v))
                , ("f_int8",aToJSON f_int8_js (s_f_int8 v))
                , ("f_int16",aToJSON f_int16_js (s_f_int16 v))
                , ("f_int32",aToJSON f_int32_js (s_f_int32 v))
                , ("f_int64",aToJSON f_int64_js (s_f_int64 v))
                , ("f_word8",aToJSON f_word8_js (s_f_word8 v))
                , ("f_word16",aToJSON f_word16_js (s_f_word16 v))
                , ("f_word32",aToJSON f_word32_js (s_f_word32 v))
                , ("f_word64",aToJSON f_word64_js (s_f_word64 v))
                , ("f_float",aToJSON f_float_js (s_f_float v))
                , ("f_double",aToJSON f_double_js (s_f_double v))
                , ("f_bytes",aToJSON f_bytes_js (s_f_bytes v))
                , ("f_string",aToJSON f_string_js (s_f_string v))
                , ("f_vstring",aToJSON f_vstring_js (s_f_vstring v))
                , ("f_a",aToJSON f_a_js (s_f_a v))
                , ("f_u",aToJSON f_u_js (s_f_u v))
                , ("f_t",aToJSON f_t_js (s_f_t v))
                , ("f_bint16",aToJSON f_bint16_js (s_f_bint16 v))
                ] )
            
            from (JSON.Object hm) = S 
                <$> fieldFromJSON' f_void_js "f_void" () hm
                <*> fieldFromJSON' f_bool_js "f_bool" Prelude.True hm
                <*> fieldFromJSON' f_int8_js "f_int8" (-5) hm
                <*> fieldFromJSON' f_int16_js "f_int16" (-10000) hm
                <*> fieldFromJSON' f_int32_js "f_int32" 56 hm
                <*> fieldFromJSON' f_int64_js "f_int64" 40000 hm
                <*> fieldFromJSON' f_word8_js "f_word8" 32 hm
                <*> fieldFromJSON' f_word16_js "f_word16" 50000 hm
                <*> fieldFromJSON' f_word32_js "f_word32" 124456 hm
                <*> fieldFromJSON' f_word64_js "f_word64" 2344 hm
                <*> fieldFromJSON' f_float_js "f_float" 0.5 hm
                <*> fieldFromJSON' f_double_js "f_double" 0.45 hm
                <*> fieldFromJSON' f_bytes_js "f_bytes" "hello" hm
                <*> fieldFromJSON' f_string_js "f_string" "abcd" hm
                <*> fieldFromJSON' f_vstring_js "f_vstring" [ "xy", "ab" ] hm
                <*> fieldFromJSON' f_a_js "f_a" A{ a_f_bool = Prelude.True, a_f_int = 31, a_f_string = "xyz" } hm
                <*> fieldFromJSON' f_u_js "f_u" (U_f_int 45) hm
                <*> fieldFromJSON f_t_js "f_t" hm
                <*> fieldFromJSON' f_bint16_js "f_bint16" B{ b_f_string = "yikes", b_f_t = 56, b_f_tvec = [ 1, 2, 3 ], b_f_xy = XY{ xY_x = 5, xY_y = 5 } } hm
            from _ = Prelude.Nothing

data U
    = U_f_int Data.Int.Int16
    | U_f_string T.Text
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U where
    atype _ = "test.U"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_int_js = jsonSerialiser jf
            f_string_js = jsonSerialiser jf
            
            to (U_f_int v) = JSON.Object (HM.singleton "f_int" (aToJSON f_int_js v))
            to (U_f_string v) = JSON.Object (HM.singleton "f_string" (aToJSON f_string_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "f_int" -> Prelude.fmap U_f_int (aFromJSON f_int_js v)
                    "f_string" -> Prelude.fmap U_f_string (aFromJSON f_string_js v)

data XY t = XY
    { xY_x :: t
    , xY_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkXY v_x v_y = XY v_x v_y

instance (ADLValue t) => ADLValue (XY t) where
    atype _ = T.concat
        [ "test.XY"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            x_js = jsonSerialiser jf
            y_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("x",aToJSON x_js (xY_x v))
                , ("y",aToJSON y_js (xY_y v))
                ] )
            
            from (JSON.Object hm) = XY 
                <$> fieldFromJSON x_js "x" hm
                <*> fieldFromJSON y_js "y" hm
            from _ = Prelude.Nothing