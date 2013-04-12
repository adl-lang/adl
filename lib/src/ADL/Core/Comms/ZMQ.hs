{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module ADL.Core.Comms.ZMQ(
  newEndPoint,
  ) where

import ADL.Core.Comms.Internals
import ADL.Core.Comms.Types.Internals
import qualified ADL.Core.Comms.ZMQ.Internals as I

newEndPoint :: Context -> Either Int (Int,Int) -> IO EndPoint
newEndPoint ctx port = do
  zctx <- zmqContext ctx
  I.newEndPoint zctx port
