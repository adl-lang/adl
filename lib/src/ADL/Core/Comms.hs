module ADL.Core.Comms(
  Context,
  zmqContext,
  ADL.Core.Comms.init,
  module ADL.Core.Comms.Types,
  connect
  ) where

import Control.Applicative

import ADL.Core.Value
import ADL.Core.Sink

import ADL.Core.Comms.Types
import qualified ADL.Core.Comms.Null as Null
import qualified ADL.Core.Comms.ZMQ.Internals as ZMQ
import qualified System.Log.Logger as L

newtype Context = Context ZMQ.Context

init :: IO Context
init = Context <$> ZMQ.init

instance Resource Context where
  release (Context zc) = release zc

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)
connect _ NullSink = Null.connect
connect (Context zc) s@ZMQSink{} = ZMQ.connect zc s

zmqContext :: Context -> IO ZMQ.Context
zmqContext (Context zc) = return zc