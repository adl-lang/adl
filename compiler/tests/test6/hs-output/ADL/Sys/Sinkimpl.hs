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
    
    defaultv = SerialisationType defaultv
    aToJSON f (SerialisationType v) = aToJSON f v
    aFromJSON f o = Prelude.fmap SerialisationType (aFromJSON f o)

data SinkData = SinkData
    { sinkData_transport :: TransportName
    , sinkData_address :: TransportAddr
    , sinkData_serialisation :: SerialisationType
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue SinkData where
    atype _ = "sys.sinkimpl.SinkData"
    
    defaultv = SinkData
        (TransportName "null")
        (TransportAddr_stringv "")
        (SerialisationType "json")
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("transport",aToJSON f (sinkData_transport v))
        , ("address",aToJSON f (sinkData_address v))
        , ("serialisation",aToJSON f (sinkData_serialisation v))
        ] )
    
    aFromJSON f (JSON.Object hm) = SinkData
        <$> fieldFromJSON f "transport" defaultv hm
        <*> fieldFromJSON f "address" defaultv hm
        <*> fieldFromJSON f "serialisation" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data TransportAddr
    = TransportAddr_stringv T.Text
    | TransportAddr_intv Data.Word.Word64
    | TransportAddr_arrayv [TransportAddr]
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TransportAddr where
    atype _ = "sys.sinkimpl.TransportAddr"
    
    defaultv = TransportAddr_stringv defaultv
    
    aToJSON f v = toJSONObject f (atype v) [case v of
        (TransportAddr_stringv v) -> ("stringv",aToJSON f v)
        (TransportAddr_intv v) -> ("intv",aToJSON f v)
        (TransportAddr_arrayv v) -> ("arrayv",aToJSON f v)
        ]
    
    aFromJSON f o = 
        let umap = HM.fromList
                [ ("stringv", \f v -> TransportAddr_stringv <$> aFromJSON f v)
                , ("intv", \f v -> TransportAddr_intv <$> aFromJSON f v)
                , ("arrayv", \f v -> TransportAddr_arrayv <$> aFromJSON f v)
                ]
        in unionFromJSON f umap o

newtype TransportName = TransportName { unTransportName :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TransportName where
    atype _ = "sys.sinkimpl.TransportName"
    
    defaultv = TransportName defaultv
    aToJSON f (TransportName v) = aToJSON f v
    aFromJSON f o = Prelude.fmap TransportName (aFromJSON f o)