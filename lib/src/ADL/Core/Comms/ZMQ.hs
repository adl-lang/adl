{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module ADL.Core.Comms.ZMQ(
  epOpen,
  ) where

import ADL.Core.Comms
import qualified ADL.Core.Comms.ZMQ.Internals as I

epOpen :: Context -> Int-> IO EndPoint
epOpen ctx port = do
  zctx <- zmqContext ctx
  I.epOpen zctx port
