{-# LANGUAGE OverloadedStrings #-}
module ADL.Test20(
    Person(..),
    Role(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

data Person = Person
    { person_firstName :: T.Text
    , person_lastName :: T.Text
    , person_age :: Data.Int.Int16
    , person_role :: Role
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Person where
    atype _ = "test20.Person"
    
    defaultv = Person
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            firstName_js = jsonSerialiser jf
            lastName_js = jsonSerialiser jf
            age_js = jsonSerialiser jf
            role_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("fn",aToJSON fn_js (person_firstName v))
                , ("ln",aToJSON ln_js (person_lastName v))
                , ("age",aToJSON age_js (person_age v))
                , ("role",aToJSON role_js (person_role v))
                ] )
            
            from (JSON.Object hm) = Person 
                <$> fieldFromJSON fn_js "fn" defaultv hm
                <*> fieldFromJSON ln_js "ln" defaultv hm
                <*> fieldFromJSON age_js "age" defaultv hm
                <*> fieldFromJSON role_js "role" defaultv hm
            from _ = Prelude.Nothing

data Role
    = Role_underling
    | Role_boss
    | Role_superBoss
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Role where
    atype _ = "test20.Role"
    
    defaultv = Role_underling
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            
            to Role_underling = JSON.Object (HM.singleton "u" JSON.Null)
            to Role_boss = JSON.Object (HM.singleton "b" JSON.Null)
            to Role_superBoss = JSON.Object (HM.singleton "sb" JSON.Null)
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "u" -> Prelude.Just Role_underling
                    "b" -> Prelude.Just Role_boss
                    "sb" -> Prelude.Just Role_superBoss