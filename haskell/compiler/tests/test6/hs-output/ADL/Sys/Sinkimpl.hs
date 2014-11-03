{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Sinkimpl(
    SerialisationType(..),
    SinkData(..),
    TransportAddr(..),
    TransportName(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

newtype SerialisationType = SerialisationType { unSerialisationType :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue SerialisationType where
    atype _ = "sys.sinkimpl.SerialisationType"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (SerialisationType v) = aToJSON js v
            from o = Prelude.fmap SerialisationType (aFromJSON js o)

data SinkData = SinkData
    { sinkData_transport :: TransportName
    , sinkData_address :: TransportAddr
    , sinkData_serialisation :: SerialisationType
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkSinkData  = SinkData (TransportName "null") (TransportAddr_stringv "") (SerialisationType "json")

instance ADLValue SinkData where
    atype _ = "sys.sinkimpl.SinkData"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            transport_js = jsonSerialiser jf
            address_js = jsonSerialiser jf
            serialisation_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("transport",aToJSON transport_js (sinkData_transport v))
                , ("address",aToJSON address_js (sinkData_address v))
                , ("serialisation",aToJSON serialisation_js (sinkData_serialisation v))
                ] )
            
            from (JSON.Object hm) = SinkData 
                <$> fieldFromJSON' transport_js "transport" (TransportName "null") hm
                <*> fieldFromJSON' address_js "address" (TransportAddr_stringv "") hm
                <*> fieldFromJSON' serialisation_js "serialisation" (SerialisationType "json") hm
            from _ = Prelude.Nothing

data TransportAddr
    = TransportAddr_stringv T.Text
    | TransportAddr_intv Data.Word.Word64
    | TransportAddr_arrayv [TransportAddr]
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TransportAddr where
    atype _ = "sys.sinkimpl.TransportAddr"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            stringv_js = jsonSerialiser jf
            intv_js = jsonSerialiser jf
            arrayv_js = jsonSerialiser jf
            
            to (TransportAddr_stringv v) = JSON.Object (HM.singleton "stringv" (aToJSON stringv_js v))
            to (TransportAddr_intv v) = JSON.Object (HM.singleton "intv" (aToJSON intv_js v))
            to (TransportAddr_arrayv v) = JSON.Object (HM.singleton "arrayv" (aToJSON arrayv_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "stringv" -> Prelude.fmap TransportAddr_stringv (aFromJSON stringv_js v)
                    "intv" -> Prelude.fmap TransportAddr_intv (aFromJSON intv_js v)
                    "arrayv" -> Prelude.fmap TransportAddr_arrayv (aFromJSON arrayv_js v)

newtype TransportName = TransportName { unTransportName :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TransportName where
    atype _ = "sys.sinkimpl.TransportName"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (TransportName v) = aToJSON js v
            from o = Prelude.fmap TransportName (aFromJSON js o)