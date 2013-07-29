{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink(
  Sink(..),
  TransportName,
  TransportAddr,
  SerialisationType(..)
  ) where

import Data.Monoid

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import ADL.Core.Value
import ADL.Core.Primitives()

-- | `Sink a` is a reference to a sink to which messages of type `a`
-- may be sent. Such a reference is an ADLValue and hence may be
-- serialised between processes.

type TransportName = T.Text
type TransportAddr = JSON.Value

data SerialisationType = S_JSON
  deriving (Eq,Show)

data Sink a = Sink {
  s_transport :: TransportName,
  s_addr :: TransportAddr,
  s_serialisation :: SerialisationType
  }
  deriving (Eq,Show)

instance Ord (Sink a) where
  compare s1 s2 = compare (s_transport s1) (s_transport s2) `mappend`
                  -- FIXME write an ord instance for JSON.Value
                  compare (JSON.encode (s_addr s1)) (JSON.encode (s_addr s2))

instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (defaultv::a),">"]

  defaultv = Sink "null" "" S_JSON

  aToJSON _ s = toJSONObject jf (atype s) (
    [ ("transport", aToJSON jf (s_transport s)),
      ("addr", s_addr s),
      ("serialisation", aToJSON jf (s_serialisation s)),
      ("type", aToJSON jf (atype s))
      ] )
                
  aFromJSON _ (JSON.Object hm) = do
    transport <- fieldFromJSON jf "transport" defaultv hm
    addr <- HM.lookup "addr" hm
    ser <- fieldFromJSON jf "serialisation" defaultv hm
    at <- fieldFromJSON jf "type" defaultv hm
    if at == atype (defaultv :: Sink a)
      then Just (Sink transport addr ser)
      else Nothing

instance ADLValue SerialisationType where
  atype _ = "SerialisationType"
  defaultv = S_JSON
  aToJSON _ S_JSON = JSON.String "JSON"
  aFromJSON _ (JSON.String s) | s == "JSON" = Just S_JSON
  aFromJSON _ _ = Nothing


jf :: JSONFlags
jf = JSONFlags False


