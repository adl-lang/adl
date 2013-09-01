{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Sys.Rpc(
    Rpc(..),
    RpcSvc,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

data Rpc i o = Rpc
    { rpc_params :: i
    , rpc_replyTo :: (Sink o)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue i, ADLValue o) => ADLValue (Rpc i o) where
    atype _ = T.concat
        [ "sys.rpc.Rpc"
        , "<", atype (Prelude.undefined ::i)
        , ",", atype (Prelude.undefined ::o)
        , ">" ]
    
    defaultv = Rpc
        defaultv
        defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("params",aToJSON f (rpc_params v))
        , ("replyTo",aToJSON f (rpc_replyTo v))
        ] )
    
    aFromJSON f (JSON.Object hm) = Rpc
        <$> fieldFromJSON f "params" defaultv hm
        <*> fieldFromJSON f "replyTo" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

type RpcSvc i o = (Sink (Rpc i o))