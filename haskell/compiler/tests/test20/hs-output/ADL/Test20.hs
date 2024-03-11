{-# LANGUAGE OverloadedStrings #-}
module ADL.Test20(
    Person(..),
    Role(..),
    mkPerson,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Person = Person
    { person_firstName :: T.Text
    , person_lastName :: T.Text
    , person_age :: Data.Int.Int16
    , person_role :: Role
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkPerson :: T.Text -> T.Text -> Data.Int.Int16 -> Role -> Person
mkPerson firstName lastName age role = Person firstName lastName age role

instance AdlValue Person where
    atype _ = "test20.Person"
    
    jsonGen = genObject
        [ genField "fn" person_firstName
        , genField "ln" person_lastName
        , genField "age" person_age
        , genField "role" person_role
        ]
    
    jsonParser = Person
        <$> parseField "fn"
        <*> parseField "ln"
        <*> parseField "age"
        <*> parseField "role"

data Role
    = Role_underling
    | Role_boss
    | Role_superBoss
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Role where
    atype _ = "test20.Role"
    
    jsonGen = genUnion (\jv -> case jv of
        Role_underling -> genUnionVoid "u"
        Role_boss -> genUnionVoid "b"
        Role_superBoss -> genUnionVoid "sb"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "u" -> parseUnionVoid Role_underling
        "b" -> parseUnionVoid Role_boss
        "sb" -> parseUnionVoid Role_superBoss
        _ -> parseFail "expected a discriminator for Role (u,b,sb)" 