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

-- | Reasons a connect operation may fail
data ConnectErrorCode
  -- | An appropriate transport for this sink has not been registered
  =  NoTransport
  -- | The connection could not be establised due to resource limitations
  | NoResources
  -- | The connect operation failed for some other reason.
  | ConnectFailed
  deriving (Eq,Show,Typeable)                        

type ConnectError = TransportError ConnectErrorCode

instance Resource Transport where
  release = t_close

data Connection = Connection {
  c_send :: LBS.ByteString -> IO (Either SendError ()),
  c_close :: IO ()
  }

-- | Reasons a send operation may fail
data SendErrorCode
  -- | The send operation is on a connection that has already been closed
  = ConnectionClosed
  -- | The send operation failed due to a transport specific problem
  | SendFailed
  deriving (Eq,Show,Typeable)

type SendError = TransportError SendErrorCode

instance Resource Connection where
  release = c_close

type SinkID = T.Text

-- | An Endpoint is a factory for 'LocalSink's, and manages a
-- collection of them. Endpoints are constructed by 'Transport's.
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

-- | An active connection to a 'Sink' of the given type.
data SinkConnection a = SinkConnection {
  -- | Send a value to the sink
  sc_send :: a -> IO (Either SendError ()),
  -- | Close the sink. No further communications on this
  -- sink will be possible
  sc_close :: IO ()
  }

instance Resource (SinkConnection a) where
  release = sc_close

-- | A @LocalSink a@ processes messages of type a, in the current process. To reference a @LocalSink@ from another process, you need to send it the @LocalSink@'s associated 'Sink'.
data LocalSink a = LocalSink {
  -- | The (serialisable) reference to sink 
  ls_sink :: Sink a,
  -- | Action to close the sink. Once closed, no more messages will 
  -- be received.
  ls_close :: IO ()
  }

instance Resource (LocalSink a) where
  release = ls_close

-- | Send a message to a 'Sink'
send :: (ADLValue a) => SinkConnection a -> a -> IO (Either SendError ())
send sc a = sc_send sc a

-- | Get the 'Sink' (ie reference) corresponding to a 'LocalSink'.
toSink :: LocalSink a -> Sink a
toSink = ls_sink

