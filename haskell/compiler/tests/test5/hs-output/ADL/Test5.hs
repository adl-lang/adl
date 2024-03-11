{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test5(
    Cell(..),
    List(..),
    S(..),
    S1(..),
    S10(..),
    S11(..),
    U1(..),
    U10(..),
    U11(..),
    U2(..),
    U3(..),
    U4(..),
    U5(..),
    U6(..),
    U7(..),
    U8(..),
    U9(..),
    mkCell,
    mkS,
    mkS1,
    mkS10,
    mkS11,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Nullable
import qualified Data.Aeson as JS
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Cell t = Cell
    { cell_head :: t
    , cell_tail :: (List t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCell :: t -> (List t) -> Cell t
mkCell head tail = Cell head tail

instance (AdlValue t) => AdlValue (Cell t) where
    atype _ = T.concat
        [ "test5.Cell"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "head" cell_head
        , genField "tail" cell_tail
        ]
    
    jsonParser = Cell
        <$> parseField "head"
        <*> parseField "tail"

data List t
    = L_null
    | L_cell (Cell t)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (List t) where
    atype _ = T.concat
        [ "test5.List"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genUnion (\jv -> case jv of
        L_null -> genUnionVoid "null"
        L_cell v -> genUnionValue "cell" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "null" -> parseUnionVoid L_null
        "cell" ->  parseUnionValue L_cell
        _ -> parseFail "expected a discriminator for List (null,cell)" 

data S = S
    { s_f1 :: (U9 T.Text)
    , s_f2 :: (U9 T.Text)
    , s_f3 :: (U9 T.Text)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS ::  S
mkS  = S (U9_v1 "xx") (U9_v2 100) U9_v3

instance AdlValue S where
    atype _ = "test5.S"
    
    jsonGen = genObject
        [ genField "f1" s_f1
        , genField "f2" s_f2
        , genField "f3" s_f3
        ]
    
    jsonParser = S
        <$> parseFieldDef "f1" (U9_v1 "xx")
        <*> parseFieldDef "f2" (U9_v2 100)
        <*> parseFieldDef "f3" U9_v3

data S1 = S1
    { s1_f :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS1 ::  S1
mkS1  = S1 100

instance AdlValue S1 where
    atype _ = "test5.S1"
    
    jsonGen = genObject
        [ genField "f" s1_f
        ]
    
    jsonParser = S1
        <$> parseFieldDef "f" 100

data S10 = S10
    { s10_f1 :: U10
    , s10_f2 :: (ADL.Core.Nullable.Nullable U10)
    , s10_f3 :: U10
    , s10_f4 :: (ADL.Core.Nullable.Nullable U10)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS10 ::  S10
mkS10  = S10 U10_v2 (ADL.Core.Nullable.fromValue (U10_v2)) (U10_v1 17) (ADL.Core.Nullable.fromValue ((U10_v1 17)))

instance AdlValue S10 where
    atype _ = "test5.S10"
    
    jsonGen = genObject
        [ genField "f1" s10_f1
        , genField "f2" s10_f2
        , genField "f3" s10_f3
        , genField "f4" s10_f4
        ]
    
    jsonParser = S10
        <$> parseFieldDef "f1" U10_v2
        <*> parseFieldDef "f2" (ADL.Core.Nullable.fromValue (U10_v2))
        <*> parseFieldDef "f3" (U10_v1 17)
        <*> parseFieldDef "f4" (ADL.Core.Nullable.fromValue ((U10_v1 17)))

data S11 = S11
    { s11_f1 :: U11
    , s11_f2 :: (ADL.Core.Nullable.Nullable U11)
    , s11_f3 :: U11
    , s11_f4 :: (ADL.Core.Nullable.Nullable U11)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS11 ::  S11
mkS11  = S11 U11_v2 (ADL.Core.Nullable.fromValue (U11_v2)) (U11_v1 17) (ADL.Core.Nullable.fromValue ((U11_v1 17)))

instance AdlValue S11 where
    atype _ = "test5.S11"
    
    jsonGen = genObject
        [ genField "f1" s11_f1
        , genField "f2" s11_f2
        , genField "f3" s11_f3
        , genField "f4" s11_f4
        ]
    
    jsonParser = S11
        <$> parseFieldDef "f1" U11_v2
        <*> parseFieldDef "f2" (ADL.Core.Nullable.fromValue (U11_v2))
        <*> parseFieldDef "f3" (U11_v1 17)
        <*> parseFieldDef "f4" (ADL.Core.Nullable.fromValue ((U11_v1 17)))

data U1
    = U1_v
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U1 where
    atype _ = "test5.U1"
    
    jsonGen = genUnion (\jv -> case jv of
        U1_v -> genUnionVoid "v"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" -> parseUnionVoid U1_v
        _ -> parseFail "expected a discriminator for U1 (v)" 

data U10
    = U10_v1 Data.Int.Int16
    | U10_v2
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U10 where
    atype _ = "test5.U10"
    
    jsonGen = genUnion (\jv -> case jv of
        U10_v1 v -> genUnionValue "v1" v
        U10_v2 -> genUnionVoid "v2"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v1" ->  parseUnionValue U10_v1
        "v2" -> parseUnionVoid U10_v2
        _ -> parseFail "expected a discriminator for U10 (v1,v2)" 

data U11
    = U11_v1 Data.Int.Int16
    | U11_v2
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U11 where
    atype _ = "test5.U11"
    
    jsonGen = genUnion (\jv -> case jv of
        U11_v1 v -> genUnionValue "VALUE1" v
        U11_v2 -> genUnionVoid "VALUE2"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "VALUE1" ->  parseUnionValue U11_v1
        "VALUE2" -> parseUnionVoid U11_v2
        _ -> parseFail "expected a discriminator for U11 (VALUE1,VALUE2)" 

data U2
    = U2_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U2 where
    atype _ = "test5.U2"
    
    jsonGen = genUnion (\jv -> case jv of
        U2_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U2_v
        _ -> parseFail "expected a discriminator for U2 (v)" 

data U3
    = U3_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U3 where
    atype _ = "test5.U3"
    
    jsonGen = genUnion (\jv -> case jv of
        U3_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U3_v
        _ -> parseFail "expected a discriminator for U3 (v)" 

data U4
    = U4_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U4 where
    atype _ = "test5.U4"
    
    jsonGen = genUnion (\jv -> case jv of
        U4_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U4_v
        _ -> parseFail "expected a discriminator for U4 (v)" 

data U5
    = U5_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U5 where
    atype _ = "test5.U5"
    
    jsonGen = genUnion (\jv -> case jv of
        U5_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U5_v
        _ -> parseFail "expected a discriminator for U5 (v)" 

data U6
    = U6_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U6 where
    atype _ = "test5.U6"
    
    jsonGen = genUnion (\jv -> case jv of
        U6_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U6_v
        _ -> parseFail "expected a discriminator for U6 (v)" 

data U7
    = U7_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U7 where
    atype _ = "test5.U7"
    
    jsonGen = genUnion (\jv -> case jv of
        U7_v v -> genUnionValue "v" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v" ->  parseUnionValue U7_v
        _ -> parseFail "expected a discriminator for U7 (v)" 

data U8
    = U8_v1 S1
    | U8_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U8 where
    atype _ = "test5.U8"
    
    jsonGen = genUnion (\jv -> case jv of
        U8_v1 v -> genUnionValue "v1" v
        U8_v2 v -> genUnionValue "v2" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v1" ->  parseUnionValue U8_v1
        "v2" ->  parseUnionValue U8_v2
        _ -> parseFail "expected a discriminator for U8 (v1,v2)" 

data U9 t
    = U9_v1 t
    | U9_v2 Data.Int.Int16
    | U9_v3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (U9 t) where
    atype _ = T.concat
        [ "test5.U9"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genUnion (\jv -> case jv of
        U9_v1 v -> genUnionValue "v1" v
        U9_v2 v -> genUnionValue "v2" v
        U9_v3 -> genUnionVoid "v3"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v1" ->  parseUnionValue U9_v1
        "v2" ->  parseUnionValue U9_v2
        "v3" -> parseUnionVoid U9_v3
        _ -> parseFail "expected a discriminator for U9 (v1,v2,v3)" 