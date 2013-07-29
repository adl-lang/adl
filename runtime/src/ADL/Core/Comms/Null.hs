{-# LANGUAGE OverloadedStrings #-}
module ADL.Core.Comms.Null(
  transportName,
  transport
  ) where

import qualified Data.Text as T
import qualified System.Log.Logger as L

import ADL.Core.Comms.Types

transportName :: T.Text
transportName = "null"

transport :: Transport
transport = Transport
  { t_name = transportName
  , t_connect = \_ -> return (Right connection)
  , t_close = return ()
  }

connection :: Connection
connection = Connection
  { c_send = \_ -> do
       L.debugM logger "Dropped message to null sink"
       return (Right ())
  , c_close = return ()             
  }

logger :: String
logger = "NullSink"             
