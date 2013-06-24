module ADL.Core.Comms.Null(
  connect    
  ) where

import qualified Data.Text as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Comms.Types.Internals

connect :: (ADLValue a) => IO (SinkConnection a)
connect = return (SinkConnection nullSend nullClose)
  where
    nullSend a = L.debugM logger ("Dropped message to null sink of type " ++ T.unpack (atype a))
    nullClose  = return ()

    logger = "NullSink"