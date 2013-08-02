{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Value where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM

-- | A type that is an instance of @ADLValue@ can be serialised, and has
-- a defined default value.

class ADLValue a where
  -- | A text string describing the type. The return string may only depend on
  -- the type - the parameter must be ignored.
  atype :: a -> T.Text

  -- | A default value of the given type
  defaultv :: a

  -- | Serialise a value to JSON
  aToJSON :: JSONFlags -> a -> JSON.Value

  -- | Deserialise a value from JSON. Returns Nothing if the JSON is invalid.
  aFromJSON :: JSONFlags -> JSON.Value -> Maybe a

-- | Flags controlling the JSON Serialisation.
data JSONFlags = JSONFlags {
  -- | If true, then fields corresponding to the types of structures and unions
  -- are included in the serialisation, and checked in the deserialisation.
  jf_typeNames :: Bool
  }

defaultJSONFlags :: JSONFlags
defaultJSONFlags = JSONFlags True

toJSONObject :: JSONFlags -> T.Text -> [ (T.Text,JSON.Value) ] -> JSON.Value
toJSONObject _ _ fields = JSON.Object $ HM.fromList fields

fieldFromJSON :: (ADLValue a) => JSONFlags -> T.Text -> a -> HM.HashMap T.Text JSON.Value -> Maybe a
fieldFromJSON f nme defv hm = case HM.lookup nme hm of
  (Just v) -> aFromJSON f v
  Nothing -> (Just defv)


unionFromJSON :: JSONFlags -> HM.HashMap T.Text (JSONFlags -> JSON.Value->Maybe a) ->
                 JSON.Value -> Maybe a
unionFromJSON f umap (JSON.Object hm) = decodeField (HM.toList hm)
  where
    decodeField [] = Nothing
    decodeField ((k,v):fs) = case HM.lookup k umap of
        Nothing -> decodeField fs
        Just uf -> uf f v
unionFromJSON _ _ _ = Nothing

-- Write an ADL value to a JSON file. The value will be wrapped in a JSON object if
-- required to create a valid JSON document.
aToJSONFile :: (ADLValue a) => JSONFlags -> FilePath -> a -> IO ()
aToJSONFile jf file a = LBS.writeFile file lbs
  where lbs = JSON.encode (wrapToplevel (aToJSON jf a))

-- Read and parse an ADL value from a JSON file. 
aFromJSONFile :: (ADLValue a) => JSONFlags -> FilePath -> IO (Maybe a)
aFromJSONFile jf file = do
  lbs <- LBS.readFile file
  case JSON.eitherDecode' lbs of
    (Left _) -> return Nothing
    (Right jv) -> return (aFromJSON jf (unwrapToplevel (jv)))

-- Read and parse an ADL value from a JSON file, throwing an exception
-- on failure.    
aFromJSONFile' :: forall a . (ADLValue a) => JSONFlags -> FilePath -> IO a
aFromJSONFile' jf file = do
  ma <- aFromJSONFile jf file
  case ma of
    Nothing -> ioError $ userError
      ("Unable to parse a value of type " ++
       T.unpack (atype (undefined :: a)) ++ " from " ++ file)
    (Just a) -> return a

wrapToplevel :: JSON.Value -> JSON.Value
wrapToplevel v@(JSON.Object _) = v
wrapToplevel v@(JSON.Array _) = v
wrapToplevel v = JSON.Object $ HM.fromList [("__toplevel",v)]

unwrapToplevel :: JSON.Value -> JSON.Value
unwrapToplevel v@(JSON.Object hm) = case HM.lookup "__toplevel" hm of
  Nothing -> v
  (Just v1) -> v1
unwrapToplevel v = v
  



