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

  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = JSON.Object $ case v of
        Nothing -> HM.singleton "nothing" JSON.Null
        (Just v) -> HM.singleton "just" (aToJSON js v)
      from o = do
        (key, v) <- splitUnion o
        case key of
          "nothing" -> return Nothing
          "just" -> fmap Just (aFromJSON js v)


instance (ADLValue t1, ADLValue t2) => ADLValue (Either t1 t2) where
  atype _ = T.concat
        [ "sys.types.Either"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
  jsonSerialiser jf = JSONSerialiser to from
    where
      js1 = jsonSerialiser jf
      js2 = jsonSerialiser jf
      to v = JSON.Object $ case v of
        (Left v) -> HM.singleton "left" (aToJSON js1 v)
        (Right v) -> HM.singleton "right" (aToJSON js2 v)
      from o = do
        (key, v) <- splitUnion o
        case key of
          "left" -> fmap Left (aFromJSON js1 v)
          "right" -> fmap Right (aFromJSON js2 v)

instance forall t1 t2 . (ADLValue t1, ADLValue t2) => ADLValue (t1,t2) where
  atype _ = T.concat
        [ "sys.types.Pair"
        , "<", atype (Prelude.undefined ::t1)
        , ",", atype (Prelude.undefined ::t2)
        , ">" ]
    
  jsonSerialiser jf = JSONSerialiser to from
    where
      js1 = jsonSerialiser jf
      js2 = jsonSerialiser jf
      to (v1,v2)= JSON.Object $ HM.fromList [
        ("v1",aToJSON js1 v1),
        ("v2",aToJSON js2 v2)
        ]
      from (JSON.Object hm) = do
        v1 <- fieldFromJSON js1 "v1" hm
        v2 <- fieldFromJSON js2 "v2" hm
        return (v1,v2)
      from _ = Nothing

instance (ADLValue k, Ord k, ADLValue v) => ADLValue (Map.Map k v) where
  atype _ = atype (undefined :: [(k,v)])
  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = aToJSON js (Map.toList v)
      from o = fmap Map.fromList (aFromJSON js o)

instance (Ord v, ADLValue v) => ADLValue (Set.Set v) where
  atype _ = atype (undefined :: [v])
  jsonSerialiser jf = JSONSerialiser to from
    where
      js = jsonSerialiser jf
      to v = aToJSON js (Set.toList v)
      from o = fmap Set.fromList (aFromJSON js o)
                                  

  
