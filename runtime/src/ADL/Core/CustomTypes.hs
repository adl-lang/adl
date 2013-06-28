{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.CustomTypes where

import Control.Applicative( (<$>), (<*>) )
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM

import ADL.Core.Value
import ADL.Core.Primitives

instance (ADLValue t) => ADLValue (Maybe t) where
    atype _ = T.concat
        [ "sys.types.maybe"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = Nothing
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (Nothing) -> ("nothing",JSON.Null)
        (Just v) -> ("just",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("nothing", \f v -> Nothing)
                , ("just", \f v -> Just <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

instance (ADLValue t1, ADLValue t2) => ADLValue (Either t1 t2) where
    atype _ = T.concat
        [ "sys.types.either"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
    defaultv = Left defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (Left v) -> ("left",aToJSON f v)
        (Right v) -> ("right",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("left", \f v -> Left <$> aFromJSON f v)
                , ("right", \f v -> Right <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

instance (ADLValue t1, ADLValue t2) => ADLValue (t1,t2) where
    atype _ = T.concat
        [ "sys.types.pair"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
    defaultv = (defaultv,defaultv)
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("v1",aToJSON f (fst v))
        , ("v2",aToJSON f (snd v))
        ] )
    
    aFromJSON f (JSON.Object hm) = (,)
        <$> fieldFromJSON f "v1" defaultv hm
        <*> fieldFromJSON f "v2" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

instance (ADLValue k, Ord k, ADLValue v) => ADLValue (Map.Map k v) where
    atype _ = atype (undefined :: [(k,v)])

    defaultv = Map.empty
                                  
    aToJSON f v = aToJSON f (Map.toList v)
    
    aFromJSON f v = Map.fromList <$> (aFromJSON f v)

instance (Ord v, ADLValue v) => ADLValue (Set.Set v) where
    atype _ = atype (undefined :: [v])

    defaultv = Set.empty
                                  
    aToJSON f v = aToJSON f (Set.toList v)
    
    aFromJSON f v = Set.fromList <$> (aFromJSON f v)

  