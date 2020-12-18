{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Map where

import qualified Data.Map as M
import qualified Data.Text as T

import qualified Data.Proxy
import ADL.Core.Value

-- we hand declare sys.types.MapEntry here, so that the AdlValue instance for Data.Map.Map can depend on it
data MapEntry k v = MapEntry
    { mapEntry_key :: k
    , mapEntry_value :: v
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkMapEntry :: k -> v -> MapEntry k v
mkMapEntry key value = MapEntry key value

instance (AdlValue k, AdlValue v) => AdlValue (MapEntry k v) where
    atype _ = T.concat
        [ "sys.types.MapEntry"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy k)
        , ",", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy v)
        , ">" ]
    
    jsonGen = genObject
        [ genField "k" mapEntry_key
        , genField "v" mapEntry_value
        ]
    
    jsonParser = MapEntry
        <$> parseField "k"
        <*> parseField "v"

instance (AdlValue k, Ord k, AdlValue v) => AdlValue (M.Map k v) where
  atype _ = atype (Data.Proxy.Proxy :: Data.Proxy.Proxy [MapEntry k v])
  jsonGen = JsonGen (adlToJson . map (uncurry MapEntry) . M.toList)
  jsonParser = mapFromMapEntryList <$> jsonParser

mapFromMapEntryList :: (AdlValue k, Ord k, AdlValue v) => [MapEntry k v] -> M.Map k v
mapFromMapEntryList = M.fromList . map (\(MapEntry k v) -> (k,v))
