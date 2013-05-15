{-# LANGUAGE OverloadedStrings #-}
module ADL.Test where

import ADL.Core
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

data S1 = S1
    { s1_x :: Data.Int.Int32
    , s1_y :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "test.S1"
    
    defaultv = S1 defaultv defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("x",aToJSON f (s1_x v))
        , ("y",aToJSON f (s1_y v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S1
        <$> fieldFromJSON f "x" defaultv hm
        <*> fieldFromJSON f "y" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data S2 = S2
    { s2_x :: Data.Int.Int32
    , s2_y :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S2 where
    atype _ = "test.S2"
    
    defaultv = S2 defaultv defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("x",aToJSON f (s2_x v))
        , ("y",aToJSON f (s2_y v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S2
        <$> fieldFromJSON f "x" defaultv hm
        <*> fieldFromJSON f "y" defaultv hm
    aFromJSON _ _ = Prelude.Nothing