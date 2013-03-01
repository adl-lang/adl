module ADL.Core.Comms where

import ADL.Core.Sink
import ADL.Core.Value


-- | Value capturing the state of the ADL communications
-- runtime
data ADLConnectionContext = ADLConnectionContext

-- | Initialise the ADL comms runtime. You probably only
-- want to call this once per process.
init :: IO ADLConnectionContext
init = undefined

data SinkConnection a = SinkConnection

-- | Create a new connection to a remote sink
connect :: ADLConnectionContext -> Sink a -> IO (SinkConnection a)
connect = undefined

-- | Send a message to a sink
scSend :: (ADLValue a) => SinkConnection a -> a -> IO ()
scSend = undefined

-- | Close a connection
scClose :: SinkConnection a -> IO ()
scClose = undefined

-- | A local sink is a sink where the message processing
-- is performed in this address space
data LocalSink a = LocalSink

-- | To receive messages, a communications endpoint is required.
data EndPoint = EndPoint

-- | Get the reference to a locally implemented sink.
lsSink :: LocalSink a -> Sink a
lsSink = undefined

-- | Close a local sink. No more messages will be processed.
lsClose :: LocalSink a -> IO ()
lsClose = undefined

-- | Create a new communications endpoint.
epOpen :: ADLConnectionContext -> IO EndPoint
epOpen = undefined
   -- open a zeromq socket for reading
   -- start a daemon thread to read from it
   -- initialise the handler map to be empty

-- | Close an endpoint. This implicitly closes all
-- local sinks associated with that endpoint.
epClose :: EndPoint -> IO ()
epClose = undefined

-- | Create a new local sink from an endpoint and a message processing
-- function. The processing function will be called in an arbitrary
-- thread chosen by the ADL communications runtime.
epNewSink :: (ADLValue a) => EndPoint -> (a -> IO ()) -> IO (LocalSink a)
epNewSink = undefined

