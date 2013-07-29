{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Sink(
  Sink(..),
  TransportName,
  TransportAddr
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

data Sink a = Sink {
  s_transport :: TransportName,
  s_addr :: TransportAddr
  }
  deriving (Eq,Show)

instance Ord (Sink a) where
  compare s1 s2 = compare (s_transport s1) (s_transport s2) `mappend`
                  -- FIXME write an ord instance for JSON.Value
                  compare (JSON.encode (s_addr s1)) (JSON.encode (s_addr s2))

instance forall a . (ADLValue a) => ADLValue (Sink a) where
  atype _ = T.concat ["sink<",atype (defaultv::a),">"]

  defaultv = Sink "null" ""

  aToJSON _ s = toJSONObject jf (atype s) (
    [ ("transport", aToJSON jf (s_transport s)),
      ("addr", s_addr s),
      ("type", aToJSON jf (atype s))
      ] )
                
  aFromJSON _ (JSON.Object hm) = do
    transport <- fieldFromJSON jf "transport" defaultv hm
    addr <- HM.lookup "addr" hm
    at <- fieldFromJSON jf "type" defaultv hm
    if at == atype (defaultv :: Sink a)
      then Just (Sink transport addr)
      else Nothing

jf :: JSONFlags
jf = JSONFlags False


