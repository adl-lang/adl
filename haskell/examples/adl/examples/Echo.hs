{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Examples.Echo(
    EchoRequest(..),
    EchoResponse(..),
    EchoServer,
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

data EchoRequest t = EchoRequest
    { echoRequest_body :: t
    , echoRequest_replyTo :: (Sink (EchoResponse t))
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (EchoRequest t) where
    atype _ = T.concat
        [ "examples.echo.EchoRequest"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = EchoRequest
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            body_js = jsonSerialiser jf
            replyTo_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("body",aToJSON body_js (echoRequest_body v))
                , ("replyTo",aToJSON replyTo_js (echoRequest_replyTo v))
                ] )
            
            from (JSON.Object hm) = EchoRequest 
                <$> fieldFromJSON body_js "body" defaultv hm
                <*> fieldFromJSON replyTo_js "replyTo" defaultv hm
            from _ = Prelude.Nothing

data EchoResponse t = EchoResponse
    { echoResponse_body :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (EchoResponse t) where
    atype _ = T.concat
        [ "examples.echo.EchoResponse"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = EchoResponse
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            body_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("body",aToJSON body_js (echoResponse_body v))
                ] )
            
            from (JSON.Object hm) = EchoResponse 
                <$> fieldFromJSON body_js "body" defaultv hm
            from _ = Prelude.Nothing

type EchoServer = (Sink (EchoRequest ()))