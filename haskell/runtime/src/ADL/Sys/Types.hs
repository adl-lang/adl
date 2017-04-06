{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Types(
    Either,
    Error,
    Map,
    Maybe,
    Nullable,
    Pair,
    Set,
) where

import ADL.Core.CustomTypes
import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude


type Either = Prelude.Either


type Error a  = Prelude.Either T.Text a


type Map k v = Map.Map k v


type Maybe = Prelude.Maybe



type Pair a b = (a,b)


type Set v = Set.Set v