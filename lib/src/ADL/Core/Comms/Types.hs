{-# LANGUAGE Rank2Types #-}
module ADL.Core.Comms.Types(
  SinkConnection,
  scCreate,
  scSend,
  scClose,
  LocalSink,
  lsCreate,
  lsClose,
  lsSink,
  EndPoint,
  epCreate,
  epNewSink,
  epClose
  ) where

import ADL.Core.Sink
import ADL.Core.Value

-- | A connection to a sink
data SinkConnection a = SinkConnection {
  sc_send :: a -> IO (),
  sc_close :: IO ()
  }

scCreate :: (a -> IO ()) -> IO () -> SinkConnection a
scCreate = SinkConnection

-- | Send a message to a sink
scSend :: (ADLValue a) => SinkConnection a -> a -> IO ()
scSend sc a = sc_send sc a

-- | Close a connection
scClose :: SinkConnection a -> IO ()
scClose sc = sc_close sc

-- | A local sink is a sink where the message processing
-- is performed in this address space
data LocalSink a = LocalSink {
  ls_sink :: Sink a,
  ls_close :: IO ()
  }

lsCreate :: Sink a -> IO () -> LocalSink a
lsCreate = LocalSink

-- | Get the reference to a locally implemented sink.
lsSink :: LocalSink a -> Sink a
lsSink = ls_sink

-- | Close a local sink. No more messages will be processed.
lsClose :: LocalSink a -> IO ()
lsClose = ls_close

data EndPoint = EndPoint {
  ep_newSink :: forall a . (ADLValue a) => (a -> IO ()) -> IO (LocalSink a),
  ep_close :: IO ()
}  

epCreate :: (forall a . (ADLValue a) => (a -> IO ()) -> IO (LocalSink a))
         -> IO ()
         -> EndPoint
epCreate = EndPoint

epNewSink :: forall a . (ADLValue a) => EndPoint -> (a -> IO ()) -> IO (LocalSink a)
epNewSink = ep_newSink

epClose :: EndPoint -> IO ()
epClose = ep_close