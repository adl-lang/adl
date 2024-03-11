{-# LANGUAGE OverloadedStrings #-}
module ADL.Test29(
    Test(..),
    mkTest,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Test = Test
    { test_foo :: (StringMap T.Text)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkTest ::  Test
mkTest  = Test (stringMapFromList [(" ", "baz"), ("\"", "baz"), ("$", "bar"), ("'", "baz"), ("degrees", "°")])

instance AdlValue Test where
    atype _ = "test29.Test"
    
    jsonGen = genObject
        [ genField "foo" test_foo
        ]
    
    jsonParser = Test
        <$> parseFieldDef "foo" (stringMapFromList [(" ", "baz"), ("\"", "baz"), ("$", "bar"), ("'", "baz"), ("degrees", "°")])