{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test(
    S1(..),
    U1(..),
    U2(..),
    U3(..),
    U4(..),
    U5(..),
    U6(..),
    U7(..),
    U8(..),
    U9(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

data S1 = S1
    { s1_f :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "test.S1"
    
    defaultv = S1
        100
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f",aToJSON f_js (s1_f v))
                ] )
            
            from (JSON.Object hm) = S1 
                <$> fieldFromJSON f_js "f" defaultv hm
            from _ = Prelude.Nothing

data U1
    = U1_v
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U1 where
    atype _ = "test.U1"
    
    defaultv = U1_v
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            
            to U1_v = JSON.Object (HM.singleton "v" JSON.Null)
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.Just U1_v

data U2
    = U2_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U2 where
    atype _ = "test.U2"
    
    defaultv = U2_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U2_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U2_v (aFromJSON v_js v)

data U3
    = U3_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U3 where
    atype _ = "test.U3"
    
    defaultv = U3_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U3_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U3_v (aFromJSON v_js v)

data U4
    = U4_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U4 where
    atype _ = "test.U4"
    
    defaultv = U4_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U4_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U4_v (aFromJSON v_js v)

data U5
    = U5_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U5 where
    atype _ = "test.U5"
    
    defaultv = U5_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U5_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U5_v (aFromJSON v_js v)

data U6
    = U6_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U6 where
    atype _ = "test.U6"
    
    defaultv = U6_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U6_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U6_v (aFromJSON v_js v)

data U7
    = U7_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U7 where
    atype _ = "test.U7"
    
    defaultv = U7_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U7_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v" -> Prelude.fmap U7_v (aFromJSON v_js v)

data U8
    = U8_v1 S1
    | U8_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U8 where
    atype _ = "test.U8"
    
    defaultv = U8_v1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            v2_js = jsonSerialiser jf
            
            to (U8_v1 v) = JSON.Object (HM.singleton "v1" (aToJSON v1_js v))
            to (U8_v2 v) = JSON.Object (HM.singleton "v2" (aToJSON v2_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v1" -> Prelude.fmap U8_v1 (aFromJSON v1_js v)
                    "v2" -> Prelude.fmap U8_v2 (aFromJSON v2_js v)

data U9 t
    = U9_v1 t
    | U9_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (U9 t) where
    atype _ = T.concat
        [ "test.U9"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = U9_v1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            v2_js = jsonSerialiser jf
            
            to (U9_v1 v) = JSON.Object (HM.singleton "v1" (aToJSON v1_js v))
            to (U9_v2 v) = JSON.Object (HM.singleton "v2" (aToJSON v2_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "v1" -> Prelude.fmap U9_v1 (aFromJSON v1_js v)
                    "v2" -> Prelude.fmap U9_v2 (aFromJSON v2_js v)