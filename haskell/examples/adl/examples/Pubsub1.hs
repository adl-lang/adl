{-# LANGUAGE OverloadedStrings #-}
module ADL.Examples.Pubsub1(
    MyChannelReq,
    MyMessage,
    MySubscribe,
    Pattern,
    Payload,
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Examples.Pubsub
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

type MyChannelReq = (ADL.Examples.Pubsub.ChannelReq Payload Pattern)

type MyMessage = (ADL.Examples.Pubsub.Message Payload)

type MySubscribe = (ADL.Examples.Pubsub.Subscribe Payload Pattern)

type Pattern = T.Text

type Payload = T.Text