module ADL.Core.Comms.HTTP(
  epOpen,
  ) where

import ADL.Core.Comms
import qualified ADL.Core.Comms.HTTP.Internals as I

epOpen :: Context -> Either Int (Int,Int) -> IO EndPoint
epOpen ctx port = do
  hctx <- httpContext ctx
  I.epOpen hctx port

