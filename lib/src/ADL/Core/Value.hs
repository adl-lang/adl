{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Value where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM

class ADLValue a where
  atype :: a -> T.Text

  defaultv :: a

  atoJSON :: ToJSONFlags -> a -> JSON.Value

  afromJSON :: FromJSONFlags -> JSON.Value -> Maybe a

data ToJSONFlags = ToJSONFlags
  { tjf_writeTypeNames :: Bool
  }

data FromJSONFlags = FromJSONFlags
  { fjf_checkTypeNames :: Bool
  }

toJSONObject :: ToJSONFlags -> T.Text -> [ (T.Text,JSON.Value) ] -> JSON.Value
toJSONObject flags name fields = JSON.Object $ HM.fromList fields

fieldFromJSON :: (ADLValue a) => FromJSONFlags -> T.Text -> a -> HM.HashMap T.Text JSON.Value -> Maybe a
fieldFromJSON f nme defv hm = case HM.lookup nme hm of
  (Just v) -> afromJSON f v
  Nothing -> (Just defv)


unionFromJSON :: FromJSONFlags -> HM.HashMap T.Text (FromJSONFlags -> JSON.Value->Maybe a) ->
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
aToFile :: (ADLValue a) => FilePath -> ToJSONFlags -> a -> IO ()
aToFile file tjf a = LBS.writeFile file lbs
  where lbs = JSON.encode (wrapToplevel (atoJSON tjf a))

-- Read and parse an ADL value from a JSON file. 
aFromFile :: (ADLValue a) => FilePath -> FromJSONFlags -> IO (Maybe a)
aFromFile file fjf = do
  lbs <- LBS.readFile file
  case JSON.eitherDecode' lbs of
    (Left e) -> return Nothing
    (Right jv) -> return (afromJSON fjf (unwrapToplevel (jv)))

-- Read and parse an ADL value from a JSON file, throwing an exception
-- on failure.    
aFromFile' :: forall a . (ADLValue a) => FilePath -> FromJSONFlags -> IO a
aFromFile' file fjf = do
  ma <- aFromFile file fjf
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
  (Just v) -> v
unwrapToplevel v = v
  



