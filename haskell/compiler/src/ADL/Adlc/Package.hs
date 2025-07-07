{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Package(
    AdlPackage(..),
    AdlPackageRef(..),
    mkAdlPackage,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data AdlPackage = AdlPackage
    { adlPackage_name :: T.Text
    , adlPackage_dependencies :: [AdlPackageRef]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkAdlPackage :: T.Text -> AdlPackage
mkAdlPackage name = AdlPackage name [  ]

instance AdlValue AdlPackage where
    atype _ = "adlc.package.AdlPackage"
    
    jsonGen = genObject
        [ genField "name" adlPackage_name
        , genField "dependencies" adlPackage_dependencies
        ]
    
    jsonParser = AdlPackage
        <$> parseField "name"
        <*> parseFieldDef "dependencies" [  ]

data AdlPackageRef
    = AdlPackageRef_localdir T.Text
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue AdlPackageRef where
    atype _ = "adlc.package.AdlPackageRef"
    
    jsonGen = genUnion (\jv -> case jv of
        AdlPackageRef_localdir v -> genUnionValue "localdir" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "localdir" ->  parseUnionValue AdlPackageRef_localdir
        _ -> parseFail "expected a discriminator for AdlPackageRef (localdir)" 