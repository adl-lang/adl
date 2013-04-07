{-# LANGUAGE Rank2Types #-}
module ADL.Core.Comms.Types(
  Resource(..),
  withResource,
  SinkConnection,
  scCreate,
  scSend,
  LocalSink,
  lsCreate,
  lsSink,
  EndPoint,
  epCreate,
  epNewSink
  ) where

import Control.Exception(bracket)
import qualified Data.Text as T

import ADL.Core.Sink
import ADL.Core.Value

-- | A resource that needs to be freed.
class Resource a where
  release :: a -> IO ()

withResource :: (Resource a) => IO a -> (a -> IO b) -> IO b
withResource ma fmb = bracket ma release fmb

-- | A connection to a sink
data SinkConnection a = SinkConnection {
  sc_send :: a -> IO (),
  sc_close :: IO ()
  }

instance Resource (SinkConnection a) where
  release = sc_close

scCreate :: (a -> IO ()) -> IO () -> SinkConnection a
scCreate = SinkConnection

-- | Send a message to a sink
scSend :: (ADLValue a) => SinkConnection a -> a -> IO ()
scSend sc a = sc_send sc a

-- | A local sink is a sink where the message processing
-- is performed in this address space
data LocalSink a = LocalSink {
  ls_sink :: Sink a,
  ls_close :: IO ()
  }

instance Resource (LocalSink a) where
  release = ls_close

lsCreate :: Sink a -> IO () -> LocalSink a
lsCreate = LocalSink

-- | Get the reference to a locally implemented sink.
lsSink :: LocalSink a -> Sink a
lsSink = ls_sink

type MkSink = forall a . (ADLValue a) => Maybe T.Text -> (a -> IO ()) -> IO (LocalSink a)

data EndPoint = EndPoint {
  ep_newSink :: MkSink,
  ep_close :: IO ()
}  

instance Resource EndPoint where
  release = ep_close

epCreate :: MkSink -> IO () -> EndPoint
epCreate = EndPoint

epNewSink :: EndPoint -> MkSink
epNewSink = ep_newSink
