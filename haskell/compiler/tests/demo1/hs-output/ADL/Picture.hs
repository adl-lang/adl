{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Picture(
    Circle(..),
    Picture(..),
    Rectangle(..),
    Translated(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Circle = Circle
    { circle_radius :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Circle where
    atype _ = "picture.Circle"
    
    defaultv = Circle
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            radius_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("radius",aToJSON radius_js (circle_radius v))
                ] )
            
            from (JSON.Object hm) = Circle 
                <$> fieldFromJSON radius_js "radius" defaultv hm
            from _ = Prelude.Nothing

data Picture
    = Picture_circle Circle
    | Picture_rectangle Rectangle
    | Picture_composed [Picture]
    | Picture_translated (Translated Picture)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Picture where
    atype _ = "picture.Picture"
    
    defaultv = Picture_circle defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            circle_js = jsonSerialiser jf
            rectangle_js = jsonSerialiser jf
            composed_js = jsonSerialiser jf
            translated_js = jsonSerialiser jf
            
            to (Picture_circle v) = JSON.Object (HM.singleton "circle" (aToJSON circle_js v))
            to (Picture_rectangle v) = JSON.Object (HM.singleton "rectangle" (aToJSON rectangle_js v))
            to (Picture_composed v) = JSON.Object (HM.singleton "composed" (aToJSON composed_js v))
            to (Picture_translated v) = JSON.Object (HM.singleton "translated" (aToJSON translated_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("circle",Prelude.Just v) -> Prelude.fmap Picture_circle (aFromJSON circle_js v)
                    ("rectangle",Prelude.Just v) -> Prelude.fmap Picture_rectangle (aFromJSON rectangle_js v)
                    ("composed",Prelude.Just v) -> Prelude.fmap Picture_composed (aFromJSON composed_js v)
                    ("translated",Prelude.Just v) -> Prelude.fmap Picture_translated (aFromJSON translated_js v)

data Rectangle = Rectangle
    { rectangle_width :: Prelude.Double
    , rectangle_height :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Rectangle where
    atype _ = "picture.Rectangle"
    
    defaultv = Rectangle
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            width_js = jsonSerialiser jf
            height_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("width",aToJSON width_js (rectangle_width v))
                , ("height",aToJSON height_js (rectangle_height v))
                ] )
            
            from (JSON.Object hm) = Rectangle 
                <$> fieldFromJSON width_js "width" defaultv hm
                <*> fieldFromJSON height_js "height" defaultv hm
            from _ = Prelude.Nothing

data Translated t = Translated
    { translated_xoffset :: Prelude.Double
    , translated_yoffset :: Prelude.Double
    , translated_object :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Translated t) where
    atype _ = T.concat
        [ "picture.Translated"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = Translated
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            xoffset_js = jsonSerialiser jf
            yoffset_js = jsonSerialiser jf
            object_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("xoffset",aToJSON xoffset_js (translated_xoffset v))
                , ("yoffset",aToJSON yoffset_js (translated_yoffset v))
                , ("object",aToJSON object_js (translated_object v))
                ] )
            
            from (JSON.Object hm) = Translated 
                <$> fieldFromJSON xoffset_js "xoffset" defaultv hm
                <*> fieldFromJSON yoffset_js "yoffset" defaultv hm
                <*> fieldFromJSON object_js "object" defaultv hm
            from _ = Prelude.Nothing