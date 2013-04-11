module ADL.Core.Comms.HTTP(
  newEndPoint,
  ) where

import ADL.Core.Comms
import qualified ADL.Core.Comms.HTTP.Internals as I

newEndPoint :: Context -> Either Int (Int,Int) -> IO EndPoint
newEndPoint ctx port = do
  hctx <- httpContext ctx
  I.newEndPoint hctx port

