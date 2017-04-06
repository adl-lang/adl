{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test5(
    Cell(..),
    List(..),
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
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Cell t = Cell
    { cell_head :: t
    , cell_tail :: (List t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Cell t) where
    atype _ = T.concat
        [ "test5.Cell"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = Cell
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            head_js = jsonSerialiser jf
            tail_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("head",aToJSON head_js (cell_head v))
                , ("tail",aToJSON tail_js (cell_tail v))
                ] )
            
            from (JSON.Object hm) = Cell 
                <$> fieldFromJSON head_js "head" defaultv hm
                <*> fieldFromJSON tail_js "tail" defaultv hm
            from _ = Prelude.Nothing

data List t
    = List_null
    | List_cell (Cell t)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (List t) where
    atype _ = T.concat
        [ "test5.List"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = List_null
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            cell_js = jsonSerialiser jf
            
            to List_null = JSON.String "null"
            to (List_cell v) = JSON.Object (HM.singleton "cell" (aToJSON cell_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("null",Prelude.Nothing) -> Prelude.Just List_null
                    ("cell",Prelude.Just v) -> Prelude.fmap List_cell (aFromJSON cell_js v)

data S1 = S1
    { s1_f :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "test5.S1"
    
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
    atype _ = "test5.U1"
    
    defaultv = U1_v
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            
            to U1_v = JSON.String "v"
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Nothing) -> Prelude.Just U1_v

data U2
    = U2_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U2 where
    atype _ = "test5.U2"
    
    defaultv = U2_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U2_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U2_v (aFromJSON v_js v)

data U3
    = U3_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U3 where
    atype _ = "test5.U3"
    
    defaultv = U3_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U3_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U3_v (aFromJSON v_js v)

data U4
    = U4_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U4 where
    atype _ = "test5.U4"
    
    defaultv = U4_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U4_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U4_v (aFromJSON v_js v)

data U5
    = U5_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U5 where
    atype _ = "test5.U5"
    
    defaultv = U5_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U5_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U5_v (aFromJSON v_js v)

data U6
    = U6_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U6 where
    atype _ = "test5.U6"
    
    defaultv = U6_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U6_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U6_v (aFromJSON v_js v)

data U7
    = U7_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U7 where
    atype _ = "test5.U7"
    
    defaultv = U7_v defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v_js = jsonSerialiser jf
            
            to (U7_v v) = JSON.Object (HM.singleton "v" (aToJSON v_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v",Prelude.Just v) -> Prelude.fmap U7_v (aFromJSON v_js v)

data U8
    = U8_v1 S1
    | U8_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U8 where
    atype _ = "test5.U8"
    
    defaultv = U8_v1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            v2_js = jsonSerialiser jf
            
            to (U8_v1 v) = JSON.Object (HM.singleton "v1" (aToJSON v1_js v))
            to (U8_v2 v) = JSON.Object (HM.singleton "v2" (aToJSON v2_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v1",Prelude.Just v) -> Prelude.fmap U8_v1 (aFromJSON v1_js v)
                    ("v2",Prelude.Just v) -> Prelude.fmap U8_v2 (aFromJSON v2_js v)

data U9 t
    = U9_v1 t
    | U9_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (U9 t) where
    atype _ = T.concat
        [ "test5.U9"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = U9_v1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            v2_js = jsonSerialiser jf
            
            to (U9_v1 v) = JSON.Object (HM.singleton "v1" (aToJSON v1_js v))
            to (U9_v2 v) = JSON.Object (HM.singleton "v2" (aToJSON v2_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("v1",Prelude.Just v) -> Prelude.fmap U9_v1 (aFromJSON v1_js v)
                    ("v2",Prelude.Just v) -> Prelude.fmap U9_v2 (aFromJSON v2_js v)