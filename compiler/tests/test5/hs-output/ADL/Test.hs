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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("f",aToJSON f (s1_f v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S1
        <$> fieldFromJSON f "f" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data U1
    = U1_v ()
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U1 where
    atype _ = "test.U1"
    
    defaultv = U1_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U1_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U1_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U2
    = U2_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U2 where
    atype _ = "test.U2"
    
    defaultv = U2_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U2_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U2_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U3
    = U3_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U3 where
    atype _ = "test.U3"
    
    defaultv = U3_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U3_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U3_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U4
    = U4_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U4 where
    atype _ = "test.U4"
    
    defaultv = U4_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U4_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U4_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U5
    = U5_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U5 where
    atype _ = "test.U5"
    
    defaultv = U5_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U5_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U5_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U6
    = U6_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U6 where
    atype _ = "test.U6"
    
    defaultv = U6_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U6_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U6_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U7
    = U7_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U7 where
    atype _ = "test.U7"
    
    defaultv = U7_v defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U7_v v) -> ("v",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v", \f v -> U7_v <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

data U8
    = U8_v1 S1
    | U8_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue U8 where
    atype _ = "test.U8"
    
    defaultv = U8_v1 defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U8_v1 v) -> ("v1",aToJSON f v)
        (U8_v2 v) -> ("v2",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v1", \f v -> U8_v1 <$> aFromJSON f v)
                , ("v2", \f v -> U8_v2 <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

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
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (U9_v1 v) -> ("v1",aToJSON f v)
        (U9_v2 v) -> ("v2",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("v1", \f v -> U9_v1 <$> aFromJSON f v)
                , ("v2", \f v -> U9_v2 <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o