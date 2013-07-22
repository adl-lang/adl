{-# LANGUAGE Rank2Types #-}
module ADL.Core.Comms.Types(
  Transport(..),
  Connection(..),
  SinkConnection(..),
  send,
  LocalSink(..),
  toSink,
  EndPoint(..),
  newLocalSink
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import ADL.Utils.Resource

import ADL.Core.Sink
import ADL.Core.Value

----------------------------------------------------------------------

type MkSink = forall a . (ADLValue a) => Maybe T.Text -> (a -> IO ()) -> IO (LocalSink a)

data Transport = Transport {
  t_name :: TransportName,
  t_connect :: TransportAddr -> IO Connection,
  t_close :: IO ()
  }
  
instance Resource Transport where
  release = t_close

data Connection = Connection {
  c_send :: LBS.ByteString -> IO (),
  c_close :: IO ()
  }  

instance Resource Connection where
  release = c_close

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
send :: (ADLValue a) => SinkConnection a -> a -> IO ()
send sc a = sc_send sc a

-- | Get the reference to a locally implemented sink.
toSink :: LocalSink a -> Sink a
toSink = ls_sink

data EndPoint = EndPoint {
  ep_newSink :: MkSink,
  ep_close :: IO ()
}  

instance Resource EndPoint where
  release = ep_close

newLocalSink :: EndPoint -> MkSink
newLocalSink = ep_newSink
