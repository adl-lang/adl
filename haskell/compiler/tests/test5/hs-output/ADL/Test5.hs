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
    
    defaultv = Cell
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "head" cell_head
        , genField "tail" cell_tail
        ]
    
    jsonParser = Cell
        <$> parseField "head"
        <*> parseField "tail"

data List t
    = List_null
    | List_cell (Cell t)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (List t) where
    atype _ = T.concat
        [ "test5.List"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = List_null
    
    jsonGen = genUnion (\jv -> case jv of
        List_null -> genUnionVoid "null"
        List_cell v -> genUnionValue "cell" v
        )
    
    jsonParser
        =   parseUnionVoid "null" List_null
        <|> parseUnionValue "cell" List_cell

data S1 = S1
    { s1_f :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS1 ::  S1
mkS1  = S1 100

instance AdlValue S1 where
    atype _ = "test5.S1"
    
    defaultv = S1
        100
    
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
    
    defaultv = U1_v
    
    jsonGen = genUnion (\jv -> case jv of
        U1_v -> genUnionVoid "v"
        )
    
    jsonParser
        =   parseUnionVoid "v" U1_v

data U2
    = U2_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U2 where
    atype _ = "test5.U2"
    
    defaultv = U2_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U2_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U2_v

data U3
    = U3_v Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U3 where
    atype _ = "test5.U3"
    
    defaultv = U3_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U3_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U3_v

data U4
    = U4_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U4 where
    atype _ = "test5.U4"
    
    defaultv = U4_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U4_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U4_v

data U5
    = U5_v S1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U5 where
    atype _ = "test5.U5"
    
    defaultv = U5_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U5_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U5_v

data U6
    = U6_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U6 where
    atype _ = "test5.U6"
    
    defaultv = U6_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U6_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U6_v

data U7
    = U7_v U3
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U7 where
    atype _ = "test5.U7"
    
    defaultv = U7_v defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U7_v v -> genUnionValue "v" v
        )
    
    jsonParser
        =   parseUnionValue "v" U7_v

data U8
    = U8_v1 S1
    | U8_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue U8 where
    atype _ = "test5.U8"
    
    defaultv = U8_v1 defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U8_v1 v -> genUnionValue "v1" v
        U8_v2 v -> genUnionValue "v2" v
        )
    
    jsonParser
        =   parseUnionValue "v1" U8_v1
        <|> parseUnionValue "v2" U8_v2

data U9 t
    = U9_v1 t
    | U9_v2 Data.Int.Int16
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (U9 t) where
    atype _ = T.concat
        [ "test5.U9"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = U9_v1 defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        U9_v1 v -> genUnionValue "v1" v
        U9_v2 v -> genUnionValue "v2" v
        )
    
    jsonParser
        =   parseUnionValue "v1" U9_v1
        <|> parseUnionValue "v2" U9_v2