{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Exception(bracket)
import Control.Concurrent(threadDelay)
import System.Environment (getArgs)
import Control.Concurrent.STM

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import ADL.Core.Comms
import ADL.Core.Value
import ADL.Core.Sink
import ADL.Examples.Echo


echoServer rfile = do
  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (epOpen ctx 6700) epClose $ \ep -> do
      ls <- epNewSink ep (processRequest ctx)
      T.writeFile rfile (sinkToText (lsSink ls))
      putStrLn ("Wrote echo server reference to " ++ show rfile)
      threadDelay 1000000000000
  where
    processRequest :: Context -> EchoRequest () -> IO ()
    processRequest ctx req = do
      sc <- connect ctx (echoRequest_replyTo req)
      scSend sc (EchoResponse (echoRequest_body req))
  
echoClient rfile = do
  bracket ADL.Core.Comms.init close $ \ctx -> do
    bracket (epOpen ctx 6700) epClose $ \ep -> do
      ms <- fmap sinkFromText (T.readFile rfile)
      case ms of
        Nothing -> print ("Unable to read sink from " ++ rfile)
        (Just s) -> do
          sc <- connect ctx s
          (sink, getValue) <- oneShotSinkWithTimeout ep 20
          let er = EchoRequest () sink
          scSend sc er
          mv <- getValue
          case mv of
            Just (EchoResponse ()) -> putStrLn "Received response"
            Nothing -> putStrLn "Request timed out"

  where
    processResponse :: Context -> EchoResponse () -> IO ()
    processResponse = undefined

-- | Create a new sink to receive a value of type a. Return an IO
-- action that will wait for a value to arrive at that sink. The value
-- will be returned, unless a timeout occurs. Either way, the sink
-- will be closed.
oneShotSinkWithTimeout :: forall a . (ADLValue a) =>  EndPoint -> Int -> IO (Sink a, IO (Maybe a))
oneShotSinkWithTimeout ep timeout = do
  rv <- atomically $ newEmptyTMVar 
  ls <- epNewSink ep (handleResponse rv)
  return (lsSink ls,(getResponse rv ls))
  where
    handleResponse :: TMVar a -> a -> IO ()
    handleResponse v a = atomically $ putTMVar v a

    getResponse :: TMVar a -> LocalSink a -> IO (Maybe a)
    getResponse rv ls = do
      dv <- registerDelay timeout
      r <- atomically $ (Just <$> takeTMVar rv) `orElse` (tryTimeout dv)
      lsClose ls
      return r

    tryTimeout :: TVar Bool -> STM (Maybe a)
    tryTimeout dv = do
      v <- readTVar dv
      if v == False then retry else return Nothing
      
                                
   
        

      

usage = do
  putStrLn "Usage:"
  putStrLn "    echocmd server"
  putStrLn "    echocmd client"
  
main = do
  args <- getArgs
  case args of
    [] -> usage
    ["server"] -> echoServer "/tmp/server.ref"
    ["client"] -> echoClient "/tmp/server.ref"
    