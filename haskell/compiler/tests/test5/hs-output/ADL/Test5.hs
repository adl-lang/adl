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

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
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
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (U9 t) where
    atype _ = T.concat
        [ "test5.U9"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genUnion (\jv -> case jv of
        U9_v1 v -> genUnionValue "v1" v
        U9_v2 v -> genUnionValue "v2" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "v1" ->  parseUnionValue U9_v1
        "v2" ->  parseUnionValue U9_v2
        _ -> parseFail "expected a discriminator for U9 (v1,v2)" 