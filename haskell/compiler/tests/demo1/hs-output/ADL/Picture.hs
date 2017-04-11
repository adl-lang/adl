{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Picture(
    Circle(..),
    Picture(..),
    Rectangle(..),
    Translated(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Circle = Circle
    { circle_radius :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Circle where
    atype _ = "picture.Circle"
    
    defaultv = Circle
        defaultv
    
    jsonGen = genObject
        [ genField "radius" circle_radius
        ]
    
    jsonParser = Circle
        <$> parseField "radius"

data Picture
    = Picture_circle Circle
    | Picture_rectangle Rectangle
    | Picture_composed [Picture]
    | Picture_translated (Translated Picture)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Picture where
    atype _ = "picture.Picture"
    
    defaultv = Picture_circle defaultv
    
    jsonGen = genUnion (\jv -> case jv of
        Picture_circle v -> genUnionValue "circle" v
        Picture_rectangle v -> genUnionValue "rectangle" v
        Picture_composed v -> genUnionValue "composed" v
        Picture_translated v -> genUnionValue "translated" v
        )
    
    jsonParser
        =   parseUnionValue "circle" Picture_circle
        <|> parseUnionValue "rectangle" Picture_rectangle
        <|> parseUnionValue "composed" Picture_composed
        <|> parseUnionValue "translated" Picture_translated

data Rectangle = Rectangle
    { rectangle_width :: Prelude.Double
    , rectangle_height :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Rectangle where
    atype _ = "picture.Rectangle"
    
    defaultv = Rectangle
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "width" rectangle_width
        , genField "height" rectangle_height
        ]
    
    jsonParser = Rectangle
        <$> parseField "width"
        <*> parseField "height"

data Translated t = Translated
    { translated_xoffset :: Prelude.Double
    , translated_yoffset :: Prelude.Double
    , translated_object :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (Translated t) where
    atype _ = T.concat
        [ "picture.Translated"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = Translated
        defaultv
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "xoffset" translated_xoffset
        , genField "yoffset" translated_yoffset
        , genField "object" translated_object
        ]
    
    jsonParser = Translated
        <$> parseField "xoffset"
        <*> parseField "yoffset"
        <*> parseField "object"