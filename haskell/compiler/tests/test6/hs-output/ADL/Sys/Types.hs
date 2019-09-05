{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Types(
    Either,
    Error,
    Map,
    Maybe,
    Pair,
    Set,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map
import qualified Data.Proxy
import qualified Data.Set
import qualified Data.Text
import qualified Prelude


type Either a b = Prelude.Either a b


type Error a  = Prelude.Either Data.Text.Text a


type Map k v = Data.Map.Map k v


type Maybe = Prelude.Maybe


type Pair a b = (a,b)


type Set v = Data.Set.Set v