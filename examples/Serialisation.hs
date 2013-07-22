{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Serialisation where

import ADL.Utils.Resource
import ADL.Core.Value

import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified ADL.Core.Comms as AC
import qualified ADL.Core.Comms.Rpc as AC
import qualified ADL.Core.Comms.HTTP as HTTP

import ADL.Sys.Rpc
import ADL.Examples.Serialisation

import Utils

server rfile = do
    withResource AC.newContext $ \ctx -> do
      http <- HTTP.newTransport ctx
      withResource (HTTP.newEndPoint http (Left 2001)) $ \ep -> do
        ls <- AC.newLocalSink ep (Just "serialisation-test-server") (processRequest ctx)
        aToJSONFile defaultJSONFlags rfile (AC.toSink ls)
        putStrLn ("Wrote server reference to " ++ show rfile)
        threadWait

returnToCaller :: (ADLValue a) => AC.Context -> Roundtrip a -> IO ()
returnToCaller ctx rt = do
    withResource (AC.connect ctx (rpc_replyTo rt)) $ \sc -> do
      AC.send sc (rpc_params rt)

processRequest :: AC.Context -> Request -> IO ()
processRequest ctx (Request_req_void rt) = returnToCaller ctx rt
processRequest ctx (Request_req_bool rt) = returnToCaller ctx rt
processRequest ctx (Request_req_int8 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_int16 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_int32 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_int64 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_word8 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_word16 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_word32 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_word64 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_float rt) = returnToCaller ctx rt
processRequest ctx (Request_req_double rt) = returnToCaller ctx rt
processRequest ctx (Request_req_bytes rt) = returnToCaller ctx rt
processRequest ctx (Request_req_string rt) = returnToCaller ctx rt
processRequest ctx (Request_req_vector rt) = returnToCaller ctx rt
processRequest ctx (Request_req_sink rt) = returnToCaller ctx rt
processRequest ctx (Request_req_u1 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_u2 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_s1 rt) = returnToCaller ctx rt
processRequest ctx (Request_req_s2 rt) = returnToCaller ctx rt

client rfile = do
  withResource AC.newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do
      s <- aFromJSONFile' defaultJSONFlags rfile 
      withResource (AC.connect ctx s) $ \sc -> do

        -- Create some dummy sinks just for serialisation testing
        withResource (AC.newLocalSink ep Nothing dummyProcessRequest) $ \ls1 -> do
        withResource (AC.newLocalSink ep (Just "dummy") dummyProcessRequest) $ \ls2 -> do
        

        -- Primitive types
        testRoundTrip Request_req_void sc ep ()
        testRoundTrip Request_req_bool sc ep True
        testRoundTrip Request_req_bool sc ep False
        testRoundTrip Request_req_int8 sc ep 3
        testRoundTrip Request_req_int8 sc ep (-2)
        testRoundTrip Request_req_int16 sc ep 3000
        testRoundTrip Request_req_int16 sc ep (-2000)
        testRoundTrip Request_req_int32 sc ep 300000
        testRoundTrip Request_req_int32 sc ep (-200000)
        testRoundTrip Request_req_int64 sc ep 300000
        testRoundTrip Request_req_int64 sc ep (-200000)
        testRoundTrip Request_req_word8 sc ep 3
        testRoundTrip Request_req_word16 sc ep 3000
        testRoundTrip Request_req_word32 sc ep 300000
        testRoundTrip Request_req_word64 sc ep 300000
        testRoundTrip Request_req_float sc ep 34.57
        testRoundTrip Request_req_double sc ep 100.29
        testRoundTrip Request_req_bytes sc ep (BS.pack [0,1..255])
        testRoundTrip Request_req_string sc ep "Serialisation test!"

        -- vectors
        testRoundTrip Request_req_vector sc ep ["Serialisation", "test!"]

        -- sinks
        testRoundTrip Request_req_sink sc ep (AC.toSink ls1)
        testRoundTrip Request_req_sink sc ep (AC.toSink ls2)

        -- structs
        testRoundTrip Request_req_s1 sc ep (S1 46 4.5)
        testRoundTrip Request_req_s2 sc ep (S2 () (-42) ["","some","text"] (AC.toSink ls2)
                                               (S1 0 1e7) (U1_f2 11.5))

        -- unions
        testRoundTrip Request_req_u1 sc ep (U1_f1 (-1001))
        testRoundTrip Request_req_u1 sc ep (U1_f2 12.5)
        testRoundTrip Request_req_u2 sc ep (U2_f1 ())
        testRoundTrip Request_req_u2 sc ep (U2_f3 ["","some","text"])
        testRoundTrip Request_req_u2 sc ep (U2_f6 (U1_f2 42))

  where          
    dummyProcessRequest :: T.Text -> IO ()
    dummyProcessRequest _ = return ()

testRoundTrip :: (ADLValue x, ADLValue a, Eq x, Show x) =>
                 (Rpc x x -> a) -> AC.SinkConnection a -> AC.EndPoint -> x -> IO ()
testRoundTrip selectorf sc ep x = do
  mx <- AC.callRPC selectorf sc ep 10000000 x
  case mx of
    Nothing -> error "test timed out"
    (Just x') | x' == x -> putStrLn ("test for type " ++ T.unpack (atype x) ++ " passed")
              | otherwise -> error ("test failed, sent " ++ show x ++ " received " ++ show x')
      
runserver args = server "/tmp/serserver.ref"

runclient args = client "/tmp/serserver.ref"

