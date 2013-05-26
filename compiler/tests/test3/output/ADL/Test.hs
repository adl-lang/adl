{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test where

import ADL.Core
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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("f_int",aToJSON f (a_f_int v))
        , ("f_string",aToJSON f (a_f_string v))
        , ("f_bool",aToJSON f (a_f_bool v))
        ] )
    
    aFromJSON f (JSON.Object hm) = A
        <$> fieldFromJSON f "f_int" defaultv hm
        <*> fieldFromJSON f "f_string" defaultv hm
        <*> fieldFromJSON f "f_bool" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("f_t",aToJSON f (b_f_t v))
        , ("f_string",aToJSON f (b_f_string v))
        , ("f_tvec",aToJSON f (b_f_tvec v))
        , ("f_xy",aToJSON f (b_f_xy v))
        ] )
    
    aFromJSON f (JSON.Object hm) = B
        <$> fieldFromJSON f "f_t" defaultv hm
        <*> fieldFromJSON f "f_string" defaultv hm
        <*> fieldFromJSON f "f_tvec" defaultv hm
        <*> fieldFromJSON f "f_xy" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data S t = S
    { s_f_void :: ()
    , s_f_bool :: Prelude.Bool
    , s_f_int8 :: Data.Int.Int8
    , s_f_int16 :: Data.Int.Int16
    , s_f_int32 :: Data.Int.Int32
    , s_f_int64 :: Data.Int.Int64
    , s_f_uint8 :: Data.Word.Word8
    , s_f_uint16 :: Data.Word.Word16
    , s_f_uint32 :: Data.Word.Word32
    , s_f_uint64 :: Data.Word.Word64
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
        (defaultv :: (B Data.Int.Int16)) { b_f_tvec = [ 1, 2, 3 ], b_f_xy = (defaultv :: (XY Data.Int.Int16)) { xY_y = 5, xY_x = 5 }, b_f_t = 56, b_f_string = "yikes" }
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("f_void",aToJSON f (s_f_void v))
        , ("f_bool",aToJSON f (s_f_bool v))
        , ("f_int8",aToJSON f (s_f_int8 v))
        , ("f_int16",aToJSON f (s_f_int16 v))
        , ("f_int32",aToJSON f (s_f_int32 v))
        , ("f_int64",aToJSON f (s_f_int64 v))
        , ("f_uint8",aToJSON f (s_f_uint8 v))
        , ("f_uint16",aToJSON f (s_f_uint16 v))
        , ("f_uint32",aToJSON f (s_f_uint32 v))
        , ("f_uint64",aToJSON f (s_f_uint64 v))
        , ("f_float",aToJSON f (s_f_float v))
        , ("f_double",aToJSON f (s_f_double v))
        , ("f_bytes",aToJSON f (s_f_bytes v))
        , ("f_string",aToJSON f (s_f_string v))
        , ("f_vstring",aToJSON f (s_f_vstring v))
        , ("f_a",aToJSON f (s_f_a v))
        , ("f_u",aToJSON f (s_f_u v))
        , ("f_t",aToJSON f (s_f_t v))
        , ("f_bint16",aToJSON f (s_f_bint16 v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S
        <$> fieldFromJSON f "f_void" defaultv hm
        <*> fieldFromJSON f "f_bool" defaultv hm
        <*> fieldFromJSON f "f_int8" defaultv hm
        <*> fieldFromJSON f "f_int16" defaultv hm
        <*> fieldFromJSON f "f_int32" defaultv hm
        <*> fieldFromJSON f "f_int64" defaultv hm
        <*> fieldFromJSON f "f_uint8" defaultv hm
        <*> fieldFromJSON f "f_uint16" defaultv hm
        <*> fieldFromJSON f "f_uint32" defaultv hm
        <*> fieldFromJSON f "f_uint64" defaultv hm
        <*> fieldFromJSON f "f_float" defaultv hm
        <*> fieldFromJSON f "f_double" defaultv hm
        <*> fieldFromJSON f "f_bytes" defaultv hm
        <*> fieldFromJSON f "f_string" defaultv hm
        <*> fieldFromJSON f "f_vstring" defaultv hm
        <*> fieldFromJSON f "f_a" defaultv hm
        <*> fieldFromJSON f "f_u" defaultv hm
        <*> fieldFromJSON f "f_t" defaultv hm
        <*> fieldFromJSON f "f_bint16" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data U
    = U_f_int Data.Int.Int16
    | U_f_string T.Text
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U where
    atype _ = "test.U"
    
    defaultv = U_f_int defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U_f_int v) -> ("f_int",aToJSON f v)
        (U_f_string v) -> ("f_string",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("f_int", \f v -> U_f_int <$> aFromJSON f v)
                , ("f_string", \f v -> U_f_string <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("x",aToJSON f (xY_x v))
        , ("y",aToJSON f (xY_y v))
        ] )
    
    aFromJSON f (JSON.Object hm) = XY
        <$> fieldFromJSON f "x" defaultv hm
        <*> fieldFromJSON f "y" defaultv hm
    aFromJSON _ _ = Prelude.Nothing