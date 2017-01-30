{-# LANGUAGE OverloadedStrings #-}
module ADL.Examples.Serialisation(
    Request(..),
    Roundtrip,
    S1(..),
    S2(..),
    U1(..),
    U2(..),
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Sys.Rpc
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data Request
    = Request_req_void (Roundtrip ())
    | Request_req_bool (Roundtrip Prelude.Bool)
    | Request_req_int8 (Roundtrip Data.Int.Int8)
    | Request_req_int16 (Roundtrip Data.Int.Int16)
    | Request_req_int32 (Roundtrip Data.Int.Int32)
    | Request_req_int64 (Roundtrip Data.Int.Int64)
    | Request_req_word8 (Roundtrip Data.Word.Word8)
    | Request_req_word16 (Roundtrip Data.Word.Word16)
    | Request_req_word32 (Roundtrip Data.Word.Word32)
    | Request_req_word64 (Roundtrip Data.Word.Word64)
    | Request_req_float (Roundtrip Prelude.Float)
    | Request_req_double (Roundtrip Prelude.Double)
    | Request_req_bytes (Roundtrip B.ByteString)
    | Request_req_string (Roundtrip T.Text)
    | Request_req_vector (Roundtrip [T.Text])
    | Request_req_sink (Roundtrip (Sink T.Text))
    | Request_req_u1 (Roundtrip U1)
    | Request_req_u2 (Roundtrip U2)
    | Request_req_s1 (Roundtrip S1)
    | Request_req_s2 (Roundtrip S2)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Request where
    atype _ = "examples.serialisation.Request"
    
    defaultv = Request_req_void defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            req_void_js = jsonSerialiser jf
            req_bool_js = jsonSerialiser jf
            req_int8_js = jsonSerialiser jf
            req_int16_js = jsonSerialiser jf
            req_int32_js = jsonSerialiser jf
            req_int64_js = jsonSerialiser jf
            req_word8_js = jsonSerialiser jf
            req_word16_js = jsonSerialiser jf
            req_word32_js = jsonSerialiser jf
            req_word64_js = jsonSerialiser jf
            req_float_js = jsonSerialiser jf
            req_double_js = jsonSerialiser jf
            req_bytes_js = jsonSerialiser jf
            req_string_js = jsonSerialiser jf
            req_vector_js = jsonSerialiser jf
            req_sink_js = jsonSerialiser jf
            req_u1_js = jsonSerialiser jf
            req_u2_js = jsonSerialiser jf
            req_s1_js = jsonSerialiser jf
            req_s2_js = jsonSerialiser jf
            
            to (Request_req_void v) = JSON.Object (HM.singleton "req_void" (aToJSON req_void_js v))
            to (Request_req_bool v) = JSON.Object (HM.singleton "req_bool" (aToJSON req_bool_js v))
            to (Request_req_int8 v) = JSON.Object (HM.singleton "req_int8" (aToJSON req_int8_js v))
            to (Request_req_int16 v) = JSON.Object (HM.singleton "req_int16" (aToJSON req_int16_js v))
            to (Request_req_int32 v) = JSON.Object (HM.singleton "req_int32" (aToJSON req_int32_js v))
            to (Request_req_int64 v) = JSON.Object (HM.singleton "req_int64" (aToJSON req_int64_js v))
            to (Request_req_word8 v) = JSON.Object (HM.singleton "req_word8" (aToJSON req_word8_js v))
            to (Request_req_word16 v) = JSON.Object (HM.singleton "req_word16" (aToJSON req_word16_js v))
            to (Request_req_word32 v) = JSON.Object (HM.singleton "req_word32" (aToJSON req_word32_js v))
            to (Request_req_word64 v) = JSON.Object (HM.singleton "req_word64" (aToJSON req_word64_js v))
            to (Request_req_float v) = JSON.Object (HM.singleton "req_float" (aToJSON req_float_js v))
            to (Request_req_double v) = JSON.Object (HM.singleton "req_double" (aToJSON req_double_js v))
            to (Request_req_bytes v) = JSON.Object (HM.singleton "req_bytes" (aToJSON req_bytes_js v))
            to (Request_req_string v) = JSON.Object (HM.singleton "req_string" (aToJSON req_string_js v))
            to (Request_req_vector v) = JSON.Object (HM.singleton "req_vector" (aToJSON req_vector_js v))
            to (Request_req_sink v) = JSON.Object (HM.singleton "req_sink" (aToJSON req_sink_js v))
            to (Request_req_u1 v) = JSON.Object (HM.singleton "req_u1" (aToJSON req_u1_js v))
            to (Request_req_u2 v) = JSON.Object (HM.singleton "req_u2" (aToJSON req_u2_js v))
            to (Request_req_s1 v) = JSON.Object (HM.singleton "req_s1" (aToJSON req_s1_js v))
            to (Request_req_s2 v) = JSON.Object (HM.singleton "req_s2" (aToJSON req_s2_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("req_void",Prelude.Just v) -> Prelude.fmap Request_req_void (aFromJSON req_void_js v)
                    ("req_bool",Prelude.Just v) -> Prelude.fmap Request_req_bool (aFromJSON req_bool_js v)
                    ("req_int8",Prelude.Just v) -> Prelude.fmap Request_req_int8 (aFromJSON req_int8_js v)
                    ("req_int16",Prelude.Just v) -> Prelude.fmap Request_req_int16 (aFromJSON req_int16_js v)
                    ("req_int32",Prelude.Just v) -> Prelude.fmap Request_req_int32 (aFromJSON req_int32_js v)
                    ("req_int64",Prelude.Just v) -> Prelude.fmap Request_req_int64 (aFromJSON req_int64_js v)
                    ("req_word8",Prelude.Just v) -> Prelude.fmap Request_req_word8 (aFromJSON req_word8_js v)
                    ("req_word16",Prelude.Just v) -> Prelude.fmap Request_req_word16 (aFromJSON req_word16_js v)
                    ("req_word32",Prelude.Just v) -> Prelude.fmap Request_req_word32 (aFromJSON req_word32_js v)
                    ("req_word64",Prelude.Just v) -> Prelude.fmap Request_req_word64 (aFromJSON req_word64_js v)
                    ("req_float",Prelude.Just v) -> Prelude.fmap Request_req_float (aFromJSON req_float_js v)
                    ("req_double",Prelude.Just v) -> Prelude.fmap Request_req_double (aFromJSON req_double_js v)
                    ("req_bytes",Prelude.Just v) -> Prelude.fmap Request_req_bytes (aFromJSON req_bytes_js v)
                    ("req_string",Prelude.Just v) -> Prelude.fmap Request_req_string (aFromJSON req_string_js v)
                    ("req_vector",Prelude.Just v) -> Prelude.fmap Request_req_vector (aFromJSON req_vector_js v)
                    ("req_sink",Prelude.Just v) -> Prelude.fmap Request_req_sink (aFromJSON req_sink_js v)
                    ("req_u1",Prelude.Just v) -> Prelude.fmap Request_req_u1 (aFromJSON req_u1_js v)
                    ("req_u2",Prelude.Just v) -> Prelude.fmap Request_req_u2 (aFromJSON req_u2_js v)
                    ("req_s1",Prelude.Just v) -> Prelude.fmap Request_req_s1 (aFromJSON req_s1_js v)
                    ("req_s2",Prelude.Just v) -> Prelude.fmap Request_req_s2 (aFromJSON req_s2_js v)

data S1 = S1
    { s1_f1 :: Data.Int.Int32
    , s1_f2 :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "examples.serialisation.S1"
    
    defaultv = S1
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s1_f1 v))
                , ("f2",aToJSON f2_js (s1_f2 v))
                ] )
            
            from (JSON.Object hm) = S1 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
            from _ = Prelude.Nothing

data S2 = S2
    { s2_f1 :: ()
    , s2_f2 :: Data.Int.Int32
    , s2_f3 :: [T.Text]
    , s2_f4 :: (Sink T.Text)
    , s2_f5 :: S1
    , s2_f6 :: U1
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S2 where
    atype _ = "examples.serialisation.S2"
    
    defaultv = S2
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            f3_js = jsonSerialiser jf
            f4_js = jsonSerialiser jf
            f5_js = jsonSerialiser jf
            f6_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s2_f1 v))
                , ("f2",aToJSON f2_js (s2_f2 v))
                , ("f3",aToJSON f3_js (s2_f3 v))
                , ("f4",aToJSON f4_js (s2_f4 v))
                , ("f5",aToJSON f5_js (s2_f5 v))
                , ("f6",aToJSON f6_js (s2_f6 v))
                ] )
            
            from (JSON.Object hm) = S2 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
                <*> fieldFromJSON f3_js "f3" defaultv hm
                <*> fieldFromJSON f4_js "f4" defaultv hm
                <*> fieldFromJSON f5_js "f5" defaultv hm
                <*> fieldFromJSON f6_js "f6" defaultv hm
            from _ = Prelude.Nothing

data U1
    = U1_f1 Data.Int.Int32
    | U1_f2 Prelude.Double
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U1 where
    atype _ = "examples.serialisation.U1"
    
    defaultv = U1_f1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to (U1_f1 v) = JSON.Object (HM.singleton "f1" (aToJSON f1_js v))
            to (U1_f2 v) = JSON.Object (HM.singleton "f2" (aToJSON f2_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("f1",Prelude.Just v) -> Prelude.fmap U1_f1 (aFromJSON f1_js v)
                    ("f2",Prelude.Just v) -> Prelude.fmap U1_f2 (aFromJSON f2_js v)

data U2
    = U2_f1
    | U2_f2 Data.Int.Int32
    | U2_f3 [T.Text]
    | U2_f4 (Sink T.Text)
    | U2_f5 S1
    | U2_f6 U1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U2 where
    atype _ = "examples.serialisation.U2"
    
    defaultv = U2_f1
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f2_js = jsonSerialiser jf
            f3_js = jsonSerialiser jf
            f4_js = jsonSerialiser jf
            f5_js = jsonSerialiser jf
            f6_js = jsonSerialiser jf
            
            to U2_f1 = JSON.String "f1"
            to (U2_f2 v) = JSON.Object (HM.singleton "f2" (aToJSON f2_js v))
            to (U2_f3 v) = JSON.Object (HM.singleton "f3" (aToJSON f3_js v))
            to (U2_f4 v) = JSON.Object (HM.singleton "f4" (aToJSON f4_js v))
            to (U2_f5 v) = JSON.Object (HM.singleton "f5" (aToJSON f5_js v))
            to (U2_f6 v) = JSON.Object (HM.singleton "f6" (aToJSON f6_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("f1",Prelude.Nothing) -> Prelude.Just U2_f1
                    ("f2",Prelude.Just v) -> Prelude.fmap U2_f2 (aFromJSON f2_js v)
                    ("f3",Prelude.Just v) -> Prelude.fmap U2_f3 (aFromJSON f3_js v)
                    ("f4",Prelude.Just v) -> Prelude.fmap U2_f4 (aFromJSON f4_js v)
                    ("f5",Prelude.Just v) -> Prelude.fmap U2_f5 (aFromJSON f5_js v)
                    ("f6",Prelude.Just v) -> Prelude.fmap U2_f6 (aFromJSON f6_js v)

type Roundtrip t = (ADL.Sys.Rpc.Rpc t t)