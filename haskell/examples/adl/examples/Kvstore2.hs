{-# LANGUAGE OverloadedStrings #-}
module ADL.Examples.Kvstore2(
    AuthenticateReq,
    Authenticator,
    Credentials(..),
    KVPair,
    KVRequest(..),
    KVService,
    Key,
    Pattern,
    QueryResults,
    User(..),
    UserDetails,
    Value,
) where

import ADL.Core.Primitives
import ADL.Core.Sink
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Sys.Rpc
import qualified ADL.Sys.Types
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

type AuthenticateReq = (ADL.Sys.Rpc.Rpc Credentials (ADL.Sys.Types.Error KVService))

type Authenticator = (Sink AuthenticateReq)

data Credentials = Credentials
    { credentials_username :: T.Text
    , credentials_password :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Credentials where
    atype _ = "examples.kvstore2.Credentials"
    
    defaultv = Credentials
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            username_js = jsonSerialiser jf
            password_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("username",aToJSON username_js (credentials_username v))
                , ("password",aToJSON password_js (credentials_password v))
                ] )
            
            from (JSON.Object hm) = Credentials 
                <$> fieldFromJSON username_js "username" defaultv hm
                <*> fieldFromJSON password_js "password" defaultv hm
            from _ = Prelude.Nothing

type KVPair = (ADL.Sys.Types.Pair Key Value)

data KVRequest
    = KVRequest_put (ADL.Sys.Rpc.Rpc KVPair (ADL.Sys.Types.Error ()))
    | KVRequest_delete (ADL.Sys.Rpc.Rpc Key (ADL.Sys.Types.Error ()))
    | KVRequest_query (ADL.Sys.Rpc.Rpc Pattern QueryResults)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue KVRequest where
    atype _ = "examples.kvstore2.KVRequest"
    
    defaultv = KVRequest_put defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            put_js = jsonSerialiser jf
            delete_js = jsonSerialiser jf
            query_js = jsonSerialiser jf
            
            to (KVRequest_put v) = JSON.Object (HM.singleton "put" (aToJSON put_js v))
            to (KVRequest_delete v) = JSON.Object (HM.singleton "delete" (aToJSON delete_js v))
            to (KVRequest_query v) = JSON.Object (HM.singleton "query" (aToJSON query_js v))
            
            from o = do
                u <- splitUnion o
                case u of
                    ("put",Prelude.Just v) -> Prelude.fmap KVRequest_put (aFromJSON put_js v)
                    ("delete",Prelude.Just v) -> Prelude.fmap KVRequest_delete (aFromJSON delete_js v)
                    ("query",Prelude.Just v) -> Prelude.fmap KVRequest_query (aFromJSON query_js v)

type KVService = (Sink KVRequest)

type Key = T.Text

type Pattern = T.Text

type QueryResults = [KVPair]

data User = User
    { user_credentials :: Credentials
    , user_has_write_access :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue User where
    atype _ = "examples.kvstore2.User"
    
    defaultv = User
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            credentials_js = jsonSerialiser jf
            has_write_access_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("credentials",aToJSON credentials_js (user_credentials v))
                , ("has_write_access",aToJSON has_write_access_js (user_has_write_access v))
                ] )
            
            from (JSON.Object hm) = User 
                <$> fieldFromJSON credentials_js "credentials" defaultv hm
                <*> fieldFromJSON has_write_access_js "has_write_access" defaultv hm
            from _ = Prelude.Nothing

type UserDetails = [User]

type Value = T.Text