{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.StringMap(
  StringMap(..),
  fromList,
  toList,
  insert,
  delete,
  lookup,
  elems,
  empty,
  singleton
) where

-- Newtype wrapper around a map with Text keys

import Prelude hiding(lookup)

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Semigroup as S

import ADL.Core.Value
import Data.Monoid
import Data.Proxy

newtype StringMap a = StringMap {toMap :: M.Map T.Text a}
  deriving (Eq,Ord,Show)

instance S.Semigroup (StringMap a) where
  (StringMap m1) <> (StringMap m2) = StringMap (m1 <> m2)

instance Monoid (StringMap a) where
  mempty = StringMap mempty
  mappend = (S.<>)  -- redundant from ghc 8.4

instance forall a . (AdlValue a) => AdlValue (StringMap a) where
  atype _ = T.concat ["StringMap<",atype (Proxy :: Proxy a),">"]
  jsonGen = JsonGen (JS.Object . HM.fromList . (map toPair) . M.toList . toMap)
    where
      toPair (k,a) = (k,adlToJson a)

  jsonParser = withJsonObject $ \ctx hm ->
    let parse (k,jv) = (\jv -> (k,jv)) <$> runJsonParser jsonParser (ParseField k:ctx) jv
    in (StringMap . M.fromList) <$> traverse parse (HM.toList hm)

singleton :: T.Text -> a -> StringMap a
singleton key value = StringMap (M.singleton key value)

lookup :: T.Text -> StringMap a -> Maybe a
lookup key (StringMap map) = M.lookup key map

insert :: T.Text -> a -> StringMap a -> StringMap a
insert key a (StringMap map) = StringMap (M.insert key a map)

delete :: T.Text -> StringMap a -> StringMap a
delete key (StringMap map) = StringMap (M.delete key map)

fromList :: [(T.Text,a)] -> StringMap a
fromList = StringMap . M.fromList

toList :: StringMap a -> [(T.Text,a)]
toList (StringMap m) = M.toList m

elems :: StringMap a -> [a]
elems (StringMap m) = M.elems m

empty :: StringMap a
empty = mempty
