module ADL.Core.Comms(
  Context,
  zmqContext,
  httpContext,
  ADL.Core.Comms.init,
  module ADL.Core.Comms.Types,
  connect
  ) where

import Control.Applicative
import Control.Concurrent.STM

import Utils.DelayIO

import ADL.Core.Value
import ADL.Core.Sink

import ADL.Core.Comms.Types
import qualified ADL.Core.Comms.Null as Null
import qualified ADL.Core.Comms.ZMQ.Internals as ZMQ
import qualified ADL.Core.Comms.HTTP.Internals as HTTP
import qualified System.Log.Logger as L

data Context = Context (DelayIO ZMQ.Context) (DelayIO HTTP.Context)

init :: IO Context
init = (Context <$> delayIO ZMQ.init <*> delayIO HTTP.init)

instance Resource Context where
  release (Context zmq http) = do
    dioState zmq >>= maybe (return ()) release
    dioState http >>= maybe (return ()) release

-- | Create a new connection to a remote sink
connect :: (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)
connect _ NullSink = Null.connect
connect c s@ZMQSink{} = do
     c1 <- zmqContext c
     ZMQ.connect c1 s
     
connect c s@HTTPSink{} = do
     c1 <- httpContext c
     HTTP.connect c1 s

zmqContext :: Context -> IO ZMQ.Context
zmqContext (Context zmq _) = dioGet zmq

httpContext :: Context -> IO HTTP.Context
httpContext (Context _ http) = dioGet http