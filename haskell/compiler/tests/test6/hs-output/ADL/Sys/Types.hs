{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Types(
    Either,
    Map,
    MapEntry,
    Maybe,
    Pair,
    Result,
    Set,
) where

import ADL.Core
import ADL.Core.Map(mapFromMapEntryList)
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Map
import qualified Data.Aeson as JS
import qualified Data.Map
import qualified Data.Proxy
import qualified Data.Set
import qualified Prelude


type Either a b = Prelude.Either a b


type Map k v = Data.Map.Map k v


type MapEntry k v = ADL.Core.Map.MapEntry k v


type Maybe = Prelude.Maybe


type Pair a b = (a,b)


type Result a b = Prelude.Either b a


type Set v = Data.Set.Set v