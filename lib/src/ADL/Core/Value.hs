{-# LANGUAGE OverloadedStrings #-}
module ADL.Core.Value where

import qualified Data.Text as T
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
