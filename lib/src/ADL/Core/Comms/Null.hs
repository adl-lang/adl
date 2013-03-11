module ADL.Core.Comms.Null(
  connect    
  ) where

import qualified Data.Text as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Comms.Types

connect :: (ADLValue a) => IO (SinkConnection a)
connect = return (scCreate nullSend nullClose)
  where
    nullSend a = L.debugM logger ("Dropped sent message of type " ++ T.unpack (atype a))
    nullClose  = return ()

    logger = "NullSink"