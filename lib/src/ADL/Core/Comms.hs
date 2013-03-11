module ADL.Core.Comms(
  Context,
  zmqContext,
  ADL.Core.Comms.init,
  close,
  SinkConnection,
  connect,
  scSend,
  scClose,
  LocalSink,
  lsSink,
  lsClose
  ) where

import Control.Applicative

import ADL.Core.Value
import ADL.Core.Sink

import ADL.Core.Comms.Types
import qualified ADL.Core.Comms.Null as Null
import qualified ADL.Core.Comms.ZMQ as ZMQ

newtype Context = Context ZMQ.Context

init :: IO Context
init = Context <$> ZMQ.init

close :: Context -> IO ()
close (Context zc) = ZMQ.close zc

zmqContext :: Context -> IO ZMQ.Context
zmqContext (Context zc) = return zc

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)
connect _ NullSink = Null.connect
connect (Context zc) s@ZMQSink{} = ZMQ.connect zc s
