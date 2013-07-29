{-# LANGUAGE ScopedTypeVariables #-}
module ADL.Core.Comms.Serialisation(
  SerialisationType(..),
  serialise,
  deserialise
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified Data.Text as T

import ADL.Core.Sink
import ADL.Core.Value

serialise :: (ADLValue a) => SerialisationType -> a -> LBS.ByteString
serialise S_JSON = serialiseJSON

deserialise :: (ADLValue a) => SerialisationType -> LBS.ByteString -> Either String a
deserialise S_JSON = deserialiseJSON

-- Messages needs to be packed in a single element JSON array
-- so that we can carrt arbtrary JSON values.

serialiseJSON :: (ADLValue a) => a -> LBS.ByteString
serialiseJSON a = JSON.encode $ JSON.Array $ V.fromList [aToJSON defaultJSONFlags a]

deserialiseJSON :: forall a . (ADLValue a) => LBS.ByteString -> Either String a
deserialiseJSON lbs =  do
  v <- JSON.eitherDecode' lbs
  case v of
    (JSON.Array a) -> case V.toList a of
      [j] -> case aFromJSON defaultJSONFlags j of
        Nothing -> Left ("Can't parse JSON to type " ++ T.unpack (atype (defaultv::a)))
        (Just a1) -> Right a1
      _ -> Left emsg
    _ -> Left emsg
  where
    emsg = "Top level JSON object must be a single element vector"

