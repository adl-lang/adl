{-# LANGUAGE Rank2Types #-}
module ADL.Core.Comms.Types.Internals where

import qualified Data.Text as T

import ADL.Utils.Resource

import ADL.Core.Sink
import ADL.Core.Value

type MkSink = forall a . (ADLValue a) => Maybe T.Text -> (a -> IO ()) -> IO (LocalSink a)

-- | A connection to a sink
data SinkConnection a = SinkConnection {
  sc_send :: a -> IO (),
  sc_close :: IO ()
  }

instance Resource (SinkConnection a) where
  release = sc_close

-- | A local sink is a sink where the message processing
-- is performed in this address space
data LocalSink a = LocalSink {
  ls_sink :: Sink a,
  ls_close :: IO ()
  }

instance Resource (LocalSink a) where
  release = ls_close


-- | Send a message to a sink
scSend :: (ADLValue a) => SinkConnection a -> a -> IO ()
scSend sc a = sc_send sc a

-- | Get the reference to a locally implemented sink.
lsSink :: LocalSink a -> Sink a
lsSink = ls_sink

data EndPoint = EndPoint {
  ep_newSink :: MkSink,
  ep_close :: IO ()
}  

instance Resource EndPoint where
  release = ep_close

newLocalSink :: EndPoint -> MkSink
newLocalSink = ep_newSink
