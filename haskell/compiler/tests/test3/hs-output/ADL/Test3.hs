{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test3(
    A(..),
    B(..),
    E(..),
    S(..),
    U(..),
    XY(..),
    mkA,
    mkB,
    mkS,
    mkXY,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Nullable
import qualified Data.Aeson as JS
import qualified Data.ByteString as B
import qualified Data.Int
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data A = A
    { a_f_int :: Data.Int.Int16
    , a_f_string :: T.Text
    , a_f_bool :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkA :: Data.Int.Int16 -> T.Text -> A
mkA f_int f_string = A f_int f_string Prelude.False

instance AdlValue A where
    atype _ = "test3.A"
    
    jsonGen = genObject
        [ genField "f_int" a_f_int
        , genField "f_string" a_f_string
        , genField "f_bool" a_f_bool
        ]
    
    jsonParser = A
        <$> parseField "f_int"
        <*> parseField "f_string"
        <*> parseFieldDef "f_bool" Prelude.False

data B t = B
    { b_f_t :: t
    , b_f_string :: T.Text
    , b_f_tvec :: [t]
    , b_f_xy :: (XY t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkB :: t -> T.Text -> [t] -> (XY t) -> B t
mkB f_t f_string f_tvec f_xy = B f_t f_string f_tvec f_xy

instance (AdlValue t) => AdlValue (B t) where
    atype _ = T.concat
        [ "test3.B"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "f_t" b_f_t
        , genField "f_string" b_f_string
        , genField "f_tvec" b_f_tvec
        , genField "f_xy" b_f_xy
        ]
    
    jsonParser = B
        <$> parseField "f_t"
        <*> parseField "f_string"
        <*> parseField "f_tvec"
        <*> parseField "f_xy"

data E
    = E_v1
    | E_v2
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue E where
    atype _ = "test3.E"
    
    jsonGen = genUnion (\jv -> case jv of
        E_v1 -> genUnionVoid "v1"
        E_v2 -> genUnionVoid "v2"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v1" -> parseUnionVoid E_v1
        "v2" -> parseUnionVoid E_v2
        _ -> parseFail "expected a discriminator for E (v1,v2)" 

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
    , s_f_u1 :: U
    , s_f_e :: E
    , s_f_t :: t
    , s_f_bint16 :: (B Data.Int.Int16)
    , s_f_smap :: (StringMap Data.Int.Int32)
    , s_f_json1 :: JS.Value
    , s_f_json2 :: JS.Value
    , s_f_nt :: (ADL.Core.Nullable.Nullable t)
    }
    deriving (Prelude.Eq,Prelude.Show)

mkS :: t -> S t
mkS f_t = S () Prelude.True (-5) (-10000) 56 40000 32 50000 124456 2344 0.5 0.45 "hello" "abcd" [ "xy", "ab" ] (A 0 "xyz" Prelude.False) (U_f_int 45) U_f_void E_v2 f_t (B 56 "yikes" [ 1, 2, 3 ] (XY 5 5)) (stringMapFromList [("a", 45), ("b", 47)]) (Data.Maybe.fromJust (JS.decode "null")) (Data.Maybe.fromJust (JS.decode "[{\"v1\":27,\"v2\":\"abcde\"},true]")) (ADL.Core.Nullable.null)

instance (AdlValue t) => AdlValue (S t) where
    atype _ = T.concat
        [ "test3.S"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "f_void" s_f_void
        , genField "f_bool" s_f_bool
        , genField "f_int8" s_f_int8
        , genField "f_int16" s_f_int16
        , genField "f_int32" s_f_int32
        , genField "f_int64" s_f_int64
        , genField "f_word8" s_f_word8
        , genField "f_word16" s_f_word16
        , genField "f_word32" s_f_word32
        , genField "f_word64" s_f_word64
        , genField "f_float" s_f_float
        , genField "f_double" s_f_double
        , genField "f_bytes" s_f_bytes
        , genField "f_string" s_f_string
        , genField "f_vstring" s_f_vstring
        , genField "f_a" s_f_a
        , genField "f_u" s_f_u
        , genField "f_u1" s_f_u1
        , genField "f_e" s_f_e
        , genField "f_t" s_f_t
        , genField "f_bint16" s_f_bint16
        , genField "f_smap" s_f_smap
        , genField "f_json1" s_f_json1
        , genField "f_json2" s_f_json2
        , genField "f_nt" s_f_nt
        ]
    
    jsonParser = S
        <$> parseFieldDef "f_void" ()
        <*> parseFieldDef "f_bool" Prelude.True
        <*> parseFieldDef "f_int8" (-5)
        <*> parseFieldDef "f_int16" (-10000)
        <*> parseFieldDef "f_int32" 56
        <*> parseFieldDef "f_int64" 40000
        <*> parseFieldDef "f_word8" 32
        <*> parseFieldDef "f_word16" 50000
        <*> parseFieldDef "f_word32" 124456
        <*> parseFieldDef "f_word64" 2344
        <*> parseFieldDef "f_float" 0.5
        <*> parseFieldDef "f_double" 0.45
        <*> parseFieldDef "f_bytes" "hello"
        <*> parseFieldDef "f_string" "abcd"
        <*> parseFieldDef "f_vstring" [ "xy", "ab" ]
        <*> parseFieldDef "f_a" (A 0 "xyz" Prelude.False)
        <*> parseFieldDef "f_u" (U_f_int 45)
        <*> parseFieldDef "f_u1" U_f_void
        <*> parseFieldDef "f_e" E_v2
        <*> parseField "f_t"
        <*> parseFieldDef "f_bint16" (B 56 "yikes" [ 1, 2, 3 ] (XY 5 5))
        <*> parseFieldDef "f_smap" (stringMapFromList [("a", 45), ("b", 47)])
        <*> parseFieldDef "f_json1" (Data.Maybe.fromJust (JS.decode "null"))
        <*> parseFieldDef "f_json2" (Data.Maybe.fromJust (JS.decode "[{\"v1\":27,\"v2\":\"abcde\"},true]"))
        <*> parseFieldDef "f_nt" (ADL.Core.Nullable.null)

data U
    = U_f_int Data.Int.Int16
    | U_f_string T.Text
    | U_f_void
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U where
    atype _ = "test3.U"
    
    jsonGen = genUnion (\jv -> case jv of
        U_f_int v -> genUnionValue "f_int" v
        U_f_string v -> genUnionValue "f_string" v
        U_f_void -> genUnionVoid "f_void"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "f_int" ->  parseUnionValue U_f_int
        "f_string" ->  parseUnionValue U_f_string
        "f_void" -> parseUnionVoid U_f_void
        _ -> parseFail "expected a discriminator for U (f_int,f_string,f_void)" 

data XY t = XY
    { xY_x :: t
    , xY_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkXY :: t -> t -> XY t
mkXY x y = XY x y

instance (AdlValue t) => AdlValue (XY t) where
    atype _ = T.concat
        [ "test3.XY"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "x" xY_x
        , genField "y" xY_y
        ]
    
    jsonParser = XY
        <$> parseField "x"
        <*> parseField "y"