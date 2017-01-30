{-# LANGUAGE OverloadedStrings #-}
module ADL.Examples.Kvstore1(
    KVPair,
    KVRequest(..),
    KVService,
    Key,
    Pattern,
    QueryResults,
    Value,
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Sys.Rpc
import qualified ADL.Sys.Types
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

type KVPair = (ADL.Sys.Types.Pair Key Value)

data KVRequest
    = KVRequest_put (ADL.Sys.Rpc.Rpc KVPair ())
    | KVRequest_delete (ADL.Sys.Rpc.Rpc Key ())
    | KVRequest_query (ADL.Sys.Rpc.Rpc Pattern QueryResults)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue KVRequest where
    atype _ = "examples.kvstore1.KVRequest"
    
    defaultv = KVRequest_put defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            put_js = jsonSerialiser jf
            delete_js = jsonSerialiser jf
            query_js = jsonSerialiser jf
            
            to (KVRequest_put v) = JSON.Object (HM.singleton "put" (aToJSON put_js v))
            to (KVRequest_delete v) = JSON.Object (HM.singleton "delete" (aToJSON delete_js v))
            to (KVRequest_query v) = JSON.Object (HM.singleton "query" (aToJSON query_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("put",Prelude.Just v) -> Prelude.fmap KVRequest_put (aFromJSON put_js v)
                    ("delete",Prelude.Just v) -> Prelude.fmap KVRequest_delete (aFromJSON delete_js v)
                    ("query",Prelude.Just v) -> Prelude.fmap KVRequest_query (aFromJSON query_js v)

type KVService = (Sink KVRequest)

type Key = T.Text

type Pattern = T.Text

type QueryResults = [KVPair]

type Value = T.Text