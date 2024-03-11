{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test2(
    IntTree,
    S0(..),
    S1(..),
    S2(..),
    S3(..),
    S4(..),
    Tree(..),
    mkS0,
    mkS1,
    mkS2,
    mkS3,
    mkS4,
    mkTree,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type IntTree = (Tree Data.Int.Int32)

data S0 = S0
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS0 ::  S0
mkS0  = S0 

instance AdlValue S0 where
    atype _ = "test2.S0"
    
    jsonGen = genObject []
    jsonParser = Prelude.pure S0

data S1 = S1
    { s1_x :: Data.Int.Int32
    , s1_y :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS1 :: Data.Int.Int32 -> T.Text -> S1
mkS1 x y = S1 x y

instance AdlValue S1 where
    atype _ = "test2.S1"
    
    jsonGen = genObject
        [ genField "x" s1_x
        , genField "y" s1_y
        ]
    
    jsonParser = S1
        <$> parseField "x"
        <*> parseField "y"

data S2 = S2
    { s2_f1 :: T.Text
    , s2_f2 :: Prelude.Double
    , s2_f3 :: [Data.Int.Int32]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS2 :: T.Text -> Prelude.Double -> [Data.Int.Int32] -> S2
mkS2 f1 f2 f3 = S2 f1 f2 f3

instance AdlValue S2 where
    atype _ = "test2.S2"
    
    jsonGen = genObject
        [ genField "f1" s2_f1
        , genField "f2" s2_f2
        , genField "f3" s2_f3
        ]
    
    jsonParser = S2
        <$> parseField "f1"
        <*> parseField "f2"
        <*> parseField "f3"

data S3 t = S3
    { s3_f1 :: T.Text
    , s3_f2 :: Prelude.Double
    , s3_f3 :: t
    , s3_f4 :: [t]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS3 :: T.Text -> Prelude.Double -> t -> [t] -> S3 t
mkS3 f1 f2 f3 f4 = S3 f1 f2 f3 f4

instance (AdlValue t) => AdlValue (S3 t) where
    atype _ = T.concat
        [ "test2.S3"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "f1" s3_f1
        , genField "f2" s3_f2
        , genField "f3" s3_f3
        , genField "f4" s3_f4
        ]
    
    jsonParser = S3
        <$> parseField "f1"
        <*> parseField "f2"
        <*> parseField "f3"
        <*> parseField "f4"

data S4 t = S4
    { s4_f1 :: (S3 T.Text)
    , s4_f2 :: (S3 t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS4 :: (S3 T.Text) -> (S3 t) -> S4 t
mkS4 f1 f2 = S4 f1 f2

instance (AdlValue t) => AdlValue (S4 t) where
    atype _ = T.concat
        [ "test2.S4"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "f1" s4_f1
        , genField "f2" s4_f2
        ]
    
    jsonParser = S4
        <$> parseField "f1"
        <*> parseField "f2"

data Tree t = Tree
    { t_value :: t
    , t_children :: [(Tree t)]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkTree :: t -> [(Tree t)] -> Tree t
mkTree value children = Tree value children

instance (AdlValue t) => AdlValue (Tree t) where
    atype _ = T.concat
        [ "test2.Tree"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "value" t_value
        , genField "children" t_children
        ]
    
    jsonParser = Tree
        <$> parseField "value"
        <*> parseField "children"