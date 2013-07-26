{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ADL.Core.Comms.HTTP
  ( Transport(..)
  , newTransport
  ) where

import Prelude hiding (catch)
import Network(Socket,sClose)
import Network.BSD(getHostName,HostName)
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import Data.Aeson(ToJSON,FromJSON, (.:), (.=) )

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp(runSettingsSocket,Settings,defaultSettings,settingsPort)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Conduit(($$))
import qualified Data.Conduit as DC
import qualified Data.Conduit.List as DC
import qualified Data.Conduit.Network as DC
import qualified Network.HTTP.Conduit as HC

import qualified System.Log.Logger as L
import Control.Monad.Trans.Resource

import ADL.Utils.Resource
import ADL.Utils.Format
import ADL.Core.Value
import ADL.Core.Sink

import qualified ADL.Core.Comms.Types as CT
import qualified ADL.Core.Comms.Internals as CT

data Transport = Transport {
  transport :: CT.Transport,
  newEndPoint :: Either Int (Int,Int) -> IO CT.EndPoint
  }

data Addr = Addr {
  httpHost :: String,
  httpPort :: Int,
  httpPath :: T.Text
}

instance ToJSON Addr where
   toJSON (Addr host port path) = JSON.object ["host" .= host, "port" .= port, "path" .= path]

instance FromJSON Addr where
   parseJSON (JSON.Object v) = Addr <$> v .: "host" <*> v .: "port" <*> v .: "path"

transportName :: TransportName
transportName = "http"

newTransport :: CT.Context -> IO Transport
newTransport c = do
  manager <- HC.newManager HC.def
  hc <- return Transport
    { transport = CT.Transport
      { CT.t_name = transportName
      , CT.t_connect = connect1 manager
      , CT.t_close = HC.closeManager manager
      }
    , newEndPoint = newEndPoint1
    }
  CT.addTransport c (transport hc)
  return hc

----------------------------------------------------------------------

type SinksV = TVar (Map.Map T.Text (LBS.ByteString -> IO ()))

newEndPoint1 :: Either Int (Int,Int) -> IO CT.EndPoint
newEndPoint1 eport = do
  hostname <- getHostName
  (port,socket) <- bindNewSocket eport
  debugM "New endpoint at http://$1:$2" [T.pack hostname, fshow port]

  -- the mapping from sink ids to the sink processing functions
  sinksv <- atomically $ newTVar Map.empty

  -- a tmvar used to communicate between the web server and the action runner
  nextactionv <- atomically $ newEmptyTMVar

  -- start the two background threads
  warptid <- forkIO $ runWarp socket sinksv nextactionv
  forkIO (runner nextactionv)

  return (CT.EndPoint (newSink hostname port sinksv)
          (closef hostname port warptid nextactionv))
  where
    -- Attempt to bind either the given socket, or any in the specified range
    bindNewSocket :: Either Int (Int,Int) -> IO (Int,Socket)

    bindNewSocket (Left port) = do
      socket <- DC.bindPort port DC.HostAny
      return (port,socket)

    bindNewSocket (Right (port,maxPort))
      | port > maxPort = ioError (userError "Unable to find available port in range")
      | otherwise = catch (bindNewSocket (Left port)) tryNextPort
      where
        tryNextPort :: IOError -> IO (Int,Socket) -- is this the right exception??
        tryNextPort e = bindNewSocket (Right (port+1,maxPort))

    runWarp socket sinksv nextactionv =
      bracket_ (return ()) (sClose socket)
               (runWarp1 socket sinksv nextactionv)

    runWarp1 :: Socket -> SinksV -> TMVar (Maybe (IO ())) -> IO ()
    runWarp1 socket sinksv nextactionv = runSettingsSocket defaultSettings socket waiApplication
      where
        waiApplication :: Request -> ResourceT IO Response
        waiApplication req = do
          if requestMethod req /= methodPost
            then errResponse badRequest400 "Only POST requests are supported"
            else case pathInfo req of
                [id] -> do
                  body <- requestBody req $$ (fmap LBS.fromChunks DC.consume)
                  handleMessage id body
                _ -> errResponse badRequest400 "request must have a single path component"

        handleMessage :: T.Text -> LBS.ByteString -> ResourceT IO Response
        handleMessage sid body = do
          sinks <- liftIO $ atomically $ readTVar sinksv
          case Map.lookup sid sinks of
            Nothing -> errResponse notFound404 ("No handler for sink with SID " ++ show sid)
            (Just actionf) -> do
                liftIO $ atomically $ putTMVar nextactionv (Just (actionf body))
                return (responseLBS ok200 [] "")

    runner :: TMVar (Maybe (IO ())) -> IO ()
    runner nextactionv = loop
      where
        loop = do
          ma <- atomically $ takeTMVar nextactionv
          case ma of
            Nothing -> return ()
            (Just action) -> do
              Control.Exception.handle actionExceptionHandler action
              loop
          
    actionExceptionHandler :: SomeException -> IO ()
    actionExceptionHandler e = do
      L.errorM httpLogger ("Failed to complete action due to exception:" ++ show e)
      return ()

    errResponse status s = do
        liftIO $ L.errorM httpLogger ("Request error: " ++ s)
        return (responseLBS status [] "")

    newSink :: forall a . (ADLValue a) => HostName -> Int -> SinksV -> Maybe T.Text -> (LBS.ByteString -> IO ()) -> IO (CT.LocalSink a)
    newSink hostname port sinksv msid handler = do
      sid <- case msid of
        (Just sid) -> return sid
        Nothing -> fmap (T.pack . UUID.toString) UUID.nextRandom

      debugM "New sink at http://$1:$2/$3" [T.pack hostname, fshow port,sid]

      let at = atype (defaultv :: a)
          sink = Sink transportName (JSON.toJSON (Addr hostname port sid))
      atomically $ modifyTVar sinksv (Map.insert sid handler)
      return (CT.LocalSink sink (closef sid))
      where
        closef sid = do
          atomically $ modifyTVar sinksv (Map.delete sid)
          debugM "Closed sink at $1:$2/$3" [T.pack hostname, fshow port, sid]

    closef hostname port warptid nextactionv = do
      killThread warptid
      atomically $ putTMVar nextactionv Nothing
      debugM "Closed endpoint at $1:$2" [T.pack hostname, fshow port]


-- | Create a new connection to a remote sink
connect1 ::  HC.Manager -> TransportAddr -> IO CT.Connection
connect1 manager addr = do
  case JSON.fromJSON addr of
    (JSON.Error e) -> error "Unable to parse HTTP Address" -- FIXME
    (JSON.Success addr) -> return (CT.Connection (send addr) close)
  where
    send :: Addr -> LBS.ByteString -> IO ()
    send addr = \lbs -> do
      let req = req0{HC.requestBody=HC.RequestBodyLBS lbs}
      debugM "POST to http://$1:$2/$3: $4" [T.pack host, fshow port, path, formatText lbs]
      resp <- runResourceT $ HC.httpLbs req manager
      return ()
      where
        host = httpHost addr
        port = httpPort addr
        path = httpPath addr
        
        req0 = HC.def {
          HC.host = BSC8.pack host,
          HC.port = port,
          HC.path = T.encodeUtf8 path,
          HC.method = "POST"
          }

    close :: IO ()
    close = return ()

httpLogger = "HTTP"

debugM :: T.Text -> [T.Text] -> IO ()
debugM t vs = L.debugM httpLogger $ T.unpack $ template t vs
