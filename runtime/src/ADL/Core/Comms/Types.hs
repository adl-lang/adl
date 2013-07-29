{-# LANGUAGE DeriveDataTypeable #-}
module ADL.Core.Comms.Types(
  Transport(..),
  Connection(..),
  SinkID,
  EndPoint(..),
  TransportError(..),
  ConnectError, ConnectErrorCode(..),
  SendError, SendErrorCode(..),

  SinkConnection(..),
  send,
  LocalSink(..),
  toSink
  ) where

import Control.Exception(Exception)
import Data.Typeable(Typeable)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import ADL.Utils.Resource

import ADL.Core.Sink
import ADL.Core.Value

----------------------------------------------------------------------
-- Transport related types

data Transport = Transport {
  t_name :: TransportName,
  t_connect :: TransportAddr -> IO (Either ConnectError Connection),
  t_close :: IO ()
  }

data ConnectErrorCode = NoTransport
                      | NoResources
                      | ConnectFailed
  deriving (Eq,Show,Typeable)                        

type ConnectError = TransportError ConnectErrorCode

instance Resource Transport where
  release = t_close

data Connection = Connection {
  c_send :: LBS.ByteString -> IO (Either SendError ()),
  c_close :: IO ()
  }

data SendErrorCode = SendFailed
  deriving (Eq,Show,Typeable)

type SendError = TransportError SendErrorCode

instance Resource Connection where
  release = c_close

type SinkID = T.Text

data EndPoint = EndPoint {
  ep_newRawSink :: Maybe SinkID -> (LBS.ByteString -> IO ()) -> IO (TransportName,TransportAddr, IO()),
  ep_close :: IO ()
}  

instance Resource EndPoint where
  release = ep_close

-- | A transport error consists of a readable code and a text message
data TransportError etype = TransportError etype String
  deriving (Show,Typeable)

-- Errors are compared only on their codes
instance (Eq e) => Eq (TransportError e) where
  (TransportError e1 _) == (TransportError e2 _) = e1 == e2

-- The ADL Core doesn't throw these, but others may want to.
instance (Typeable err, Show err) => Exception (TransportError err)

----------------------------------------------------------------------

-- | A connection to a sink
data SinkConnection a = SinkConnection {
  sc_send :: a -> IO (Either SendError ()),
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
send :: (ADLValue a) => SinkConnection a -> a -> IO (Either SendError ())
send sc a = sc_send sc a

-- | Get the reference to a locally implemented sink.
toSink :: LocalSink a -> Sink a
toSink = ls_sink

