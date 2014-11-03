{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Sys.Rpc(
    Rpc(..),
    RpcSvc,
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
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

mkRpc v_params v_replyTo = Rpc v_params v_replyTo

instance (ADLValue i, ADLValue o) => ADLValue (Rpc i o) where
    atype _ = T.concat
        [ "sys.rpc.Rpc"
        , "<", atype (Prelude.undefined ::i)
        , ",", atype (Prelude.undefined ::o)
        , ">" ]
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            params_js = jsonSerialiser jf
            replyTo_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("params",aToJSON params_js (rpc_params v))
                , ("replyTo",aToJSON replyTo_js (rpc_replyTo v))
                ] )
            
            from (JSON.Object hm) = Rpc 
                <$> fieldFromJSON params_js "params" hm
                <*> fieldFromJSON replyTo_js "replyTo" hm
            from _ = Prelude.Nothing

type RpcSvc i o = (Sink (Rpc i o))