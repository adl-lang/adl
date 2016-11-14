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
        [ "sys.types.Maybe"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
  defaultv = Nothing

  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = JSON.Object $ case v of
        Nothing -> HM.singleton "nothing" JSON.Null
        (Just v) -> HM.singleton "just" (aToJSON js v)
      from o = do
        u <- splitUnion o
        case u of
          ("nothing",Nothing) -> return Nothing
          ("just",Just v) -> fmap Just (aFromJSON js v)


instance (ADLValue t1, ADLValue t2) => ADLValue (Either t1 t2) where
  atype _ = T.concat
        [ "sys.types.Either"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
  defaultv = Left defaultv

  jsonSerialiser jf = JSONSerialiser to from
    where
      js1 = jsonSerialiser jf
      js2 = jsonSerialiser jf
      to v = JSON.Object $ case v of
        (Left v) -> HM.singleton "left" (aToJSON js1 v)
        (Right v) -> HM.singleton "right" (aToJSON js2 v)
      from o = do
        u <- splitUnion o
        case u of
          ("left", Just v) -> fmap Left (aFromJSON js1 v)
          ("right", Just v) -> fmap Right (aFromJSON js2 v)

instance forall t1 t2 . (ADLValue t1, ADLValue t2) => ADLValue (t1,t2) where
  atype _ = T.concat
        [ "sys.types.Pair"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
  defaultv = (defaultv,defaultv)

  jsonSerialiser jf = JSONSerialiser to from
    where
      js1 = jsonSerialiser jf
      js2 = jsonSerialiser jf
      to (v1,v2)= JSON.Object $ HM.fromList [
        ("v1",aToJSON js1 v1),
        ("v2",aToJSON js2 v2)
        ]
      from (JSON.Object hm) = do
        v1 <- fieldFromJSON js1 "v1" (fst (defaultv :: (t1,t2))) hm
        v2 <- fieldFromJSON js2 "v2" (snd (defaultv :: (t1,t2))) hm
        return (v1,v2)
      from _ = Nothing

instance (ADLValue k, Ord k, ADLValue v) => ADLValue (Map.Map k v) where
  atype _ = atype (undefined :: [(k,v)])
  defaultv = Map.empty
  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = aToJSON js (Map.toList v)
      from o = fmap Map.fromList (aFromJSON js o)

instance (Ord v, ADLValue v) => ADLValue (Set.Set v) where
  atype _ = atype (undefined :: [v])
  defaultv = Set.empty
  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = aToJSON js (Set.toList v)
      from o = fmap Set.fromList (aFromJSON js o)
                                  

newtype Nullable t = Nullable (Maybe t)

instance (ADLValue t) => ADLValue (Nullable t) where
  atype _ = T.concat
        [ "sys.types.Nullable"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
  defaultv = Nullable Nothing

  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf

      to v = case v of
        (Nullable Nothing) -> JSON.Null
        (Nullable (Just v)) -> aToJSON js v
        
      from JSON.Null = return (Nullable Nothing)
      from jv        = (Nullable . Just) <$> aFromJSON js jv
