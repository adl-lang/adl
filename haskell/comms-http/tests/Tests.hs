{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import Control.Concurrent.MVar
import qualified Data.Text as T

import ADL.Utils.Resource
import ADL.Core.Value
import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Comms

import qualified ADL.Core.Comms.HTTP as HTTP

import Test.Hspec

connectE ctx s = do
    elc <- connect ctx s
    case elc of
     (Left err) -> error ("Connection error: " ++ show err)
     (Right lc) -> return lc

testSend ::(ADLValue a) => a -> IO a
testSend v = do
  resultv <- newEmptyMVar
  withResource newContext $ \ctx -> do
    http <- HTTP.newTransport ctx
    withResource (HTTP.newEndPoint http (Right (2100,2200))) $ \ep -> do
      withResource (newLocalSink ep (Just "test") (putMVar resultv)) $ \ls -> do
          withResource (connectE ctx (toSink ls)) $ \sc -> do
            send sc v
            takeMVar resultv

main :: IO ()
main = hspec $ do
  describe "http adl transport" $ do
    it "can communicate some basic types" $ do
       v <- testSend (4242::Int64)
       v `shouldBe` (4242::Int64)
       v <- testSend ("hello"::T.Text)
       v `shouldBe` ("hello"::T.Text)





