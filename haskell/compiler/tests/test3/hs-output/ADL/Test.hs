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

instance ADLValue A where
    atype _ = "test.A"
    
    defaultv = A
        defaultv
        defaultv
        defaultv
    
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
                <$> fieldFromJSON f_int_js "f_int" defaultv hm
                <*> fieldFromJSON f_string_js "f_string" defaultv hm
                <*> fieldFromJSON f_bool_js "f_bool" defaultv hm
            from _ = Prelude.Nothing

data B t = B
    { b_f_t :: t
    , b_f_string :: T.Text
    , b_f_tvec :: [t]
    , b_f_xy :: (XY t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (B t) where
    atype _ = T.concat
        [ "test.B"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = B
        defaultv
        defaultv
        defaultv
        defaultv
    
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
                <$> fieldFromJSON f_t_js "f_t" defaultv hm
                <*> fieldFromJSON f_string_js "f_string" defaultv hm
                <*> fieldFromJSON f_tvec_js "f_tvec" defaultv hm
                <*> fieldFromJSON f_xy_js "f_xy" defaultv hm
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

instance (ADLValue t) => ADLValue (S t) where
    atype _ = T.concat
        [ "test.S"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = S
        ()
        Prelude.True
        (-5)
        (-10000)
        56
        40000
        32
        50000
        124456
        2344
        0.5
        0.45
        "hello"
        "abcd"
        [ "xy", "ab" ]
        defaultv { a_f_bool = Prelude.True, a_f_string = "xyz" }
        (U_f_int 45)
        defaultv
        (defaultv :: (B Data.Int.Int16)) { b_f_string = "yikes", b_f_t = 56, b_f_tvec = [ 1, 2, 3 ], b_f_xy = (defaultv :: (XY Data.Int.Int16)) { xY_x = 5, xY_y = 5 } }
    
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
                <$> fieldFromJSON f_void_js "f_void" defaultv hm
                <*> fieldFromJSON f_bool_js "f_bool" defaultv hm
                <*> fieldFromJSON f_int8_js "f_int8" defaultv hm
                <*> fieldFromJSON f_int16_js "f_int16" defaultv hm
                <*> fieldFromJSON f_int32_js "f_int32" defaultv hm
                <*> fieldFromJSON f_int64_js "f_int64" defaultv hm
                <*> fieldFromJSON f_word8_js "f_word8" defaultv hm
                <*> fieldFromJSON f_word16_js "f_word16" defaultv hm
                <*> fieldFromJSON f_word32_js "f_word32" defaultv hm
                <*> fieldFromJSON f_word64_js "f_word64" defaultv hm
                <*> fieldFromJSON f_float_js "f_float" defaultv hm
                <*> fieldFromJSON f_double_js "f_double" defaultv hm
                <*> fieldFromJSON f_bytes_js "f_bytes" defaultv hm
                <*> fieldFromJSON f_string_js "f_string" defaultv hm
                <*> fieldFromJSON f_vstring_js "f_vstring" defaultv hm
                <*> fieldFromJSON f_a_js "f_a" defaultv hm
                <*> fieldFromJSON f_u_js "f_u" defaultv hm
                <*> fieldFromJSON f_t_js "f_t" defaultv hm
                <*> fieldFromJSON f_bint16_js "f_bint16" defaultv hm
            from _ = Prelude.Nothing

data U
    = U_f_int Data.Int.Int16
    | U_f_string T.Text
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U where
    atype _ = "test.U"
    
    defaultv = U_f_int defaultv
    
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

instance (ADLValue t) => ADLValue (XY t) where
    atype _ = T.concat
        [ "test.XY"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = XY
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            x_js = jsonSerialiser jf
            y_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("x",aToJSON x_js (xY_x v))
                , ("y",aToJSON y_js (xY_y v))
                ] )
            
            from (JSON.Object hm) = XY 
                <$> fieldFromJSON x_js "x" defaultv hm
                <*> fieldFromJSON y_js "y" defaultv hm
            from _ = Prelude.Nothing