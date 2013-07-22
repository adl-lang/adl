{-# LANGUAGE OverloadedStrings #-}
module ADL.Core.Comms.Null(
  transportName,
  transport
  ) where

import qualified Data.Text as T
import qualified System.Log.Logger as L

import ADL.Core.Value
import ADL.Core.Comms.Types

transportName :: T.Text
transportName = "null"

transport :: Transport
transport = Transport
  { t_name = transportName
  , t_connect = \addr -> return (connection)
  , t_close = return ()
  }

connection = Connection
  { c_send = \t -> L.debugM logger "Dropped message to null sink"
  , c_close = return ()             
  }

logger = "NullSink"             
