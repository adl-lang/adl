{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.CustomTypes where

import Control.Applicative( (<$>), (<*>) )
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM

import ADL.Core.Value

instance (ADLValue t) => ADLValue (Maybe t) where
    atype _ = T.concat
        [ "sys.types.maybe"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = Nothing
    
    atoJSON f v = toJSONObject f (atype v) [case v of
        (Nothing) -> ("nothing",JSON.Null)
        (Just v) -> ("just",atoJSON f v)
        ]
    
    afromJSON f o = 
        let umap = HM.fromList
                [ ("nothing", \f v -> Nothing)
                , ("just", \f v -> Just <$> afromJSON f v)
                ]
        in unionFromJSON f umap o

instance (ADLValue t1, ADLValue t2) => ADLValue (Either t1 t2) where
    atype _ = T.concat
        [ "sys.types.either"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
    defaultv = Left defaultv
    
    atoJSON f v = toJSONObject f (atype v) [case v of
        (Left v) -> ("left",atoJSON f v)
        (Right v) -> ("right",atoJSON f v)
        ]
    
    afromJSON f o = 
        let umap = HM.fromList
                [ ("left", \f v -> Left <$> afromJSON f v)
                , ("right", \f v -> Right <$> afromJSON f v)
                ]
        in unionFromJSON f umap o

type Pair t1 t2 = (t1,t2)

instance (ADLValue t1, ADLValue t2) => ADLValue (t1,t2) where
    atype _ = T.concat
        [ "sys.types.pair"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
    defaultv = (defaultv,defaultv)
    
    atoJSON f v = toJSONObject f (atype v) (
        [ ("v1",atoJSON f (fst v))
        , ("v2",atoJSON f (snd v))
        ] )
    
    afromJSON f (JSON.Object hm) = (,)
        <$> fieldFromJSON f "v1" defaultv hm
        <*> fieldFromJSON f "v2" defaultv hm
    afromJSON _ _ = Prelude.Nothing