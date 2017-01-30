{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Examples.Pubsub(
    Channel,
    ChannelReq(..),
    Message(..),
    SubsReq(..),
    Subscribe(..),
    Subscription,
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Examples.Datetime
import qualified ADL.Sys.Rpc
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

type Channel t p = (Sink (ChannelReq t p))

data ChannelReq t p
    = ChannelReq_publish (Message t)
    | ChannelReq_subscribe (ADL.Sys.Rpc.Rpc (Subscribe t p) Subscription)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t, ADLValue p) => ADLValue (ChannelReq t p) where
    atype _ = T.concat
        [ "examples.pubsub.ChannelReq"
        , "<", atype (Prelude.undefined ::t)
        , ",", atype (Prelude.undefined ::p)
        , ">" ]
    
    defaultv = ChannelReq_publish defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            publish_js = jsonSerialiser jf
            subscribe_js = jsonSerialiser jf
            
            to (ChannelReq_publish v) = JSON.Object (HM.singleton "publish" (aToJSON publish_js v))
            to (ChannelReq_subscribe v) = JSON.Object (HM.singleton "subscribe" (aToJSON subscribe_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("publish",Prelude.Just v) -> Prelude.fmap ChannelReq_publish (aFromJSON publish_js v)
                    ("subscribe",Prelude.Just v) -> Prelude.fmap ChannelReq_subscribe (aFromJSON subscribe_js v)

data Message t = Message
    { message_timestamp :: ADL.Examples.Datetime.UTCTime
    , message_payload :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Message t) where
    atype _ = T.concat
        [ "examples.pubsub.Message"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = Message
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            timestamp_js = jsonSerialiser jf
            payload_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("timestamp",aToJSON timestamp_js (message_timestamp v))
                , ("payload",aToJSON payload_js (message_payload v))
                ] )
            
            from (JSON.Object hm) = Message 
                <$> fieldFromJSON timestamp_js "timestamp" defaultv hm
                <*> fieldFromJSON payload_js "payload" defaultv hm
            from _ = Prelude.Nothing

data SubsReq
    = SubsReq_unsubscribe
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue SubsReq where
    atype _ = "examples.pubsub.SubsReq"
    
    defaultv = SubsReq_unsubscribe
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            
            to SubsReq_unsubscribe = JSON.String "unsubscribe"
            
            from o = do
                u <- splitUnion o
                case u of
                    ("unsubscribe",Prelude.Nothing) -> Prelude.Just SubsReq_unsubscribe

data Subscribe t p = Subscribe
    { subscribe_pattern :: p
    , subscribe_sendTo :: (Sink (Message t))
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t, ADLValue p) => ADLValue (Subscribe t p) where
    atype _ = T.concat
        [ "examples.pubsub.Subscribe"
        , "<", atype (Prelude.undefined ::t)
        , ",", atype (Prelude.undefined ::p)
        , ">" ]
    
    defaultv = Subscribe
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            pattern_js = jsonSerialiser jf
            sendTo_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("pattern",aToJSON pattern_js (subscribe_pattern v))
                , ("sendTo",aToJSON sendTo_js (subscribe_sendTo v))
                ] )
            
            from (JSON.Object hm) = Subscribe 
                <$> fieldFromJSON pattern_js "pattern" defaultv hm
                <*> fieldFromJSON sendTo_js "sendTo" defaultv hm
            from _ = Prelude.Nothing

type Subscription = (Sink SubsReq)