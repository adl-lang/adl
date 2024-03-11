{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test24(
    CrudReqs(..),
    PostReq(..),
    Service(..),
    mkCrudReqs,
    mkPostReq,
    mkService,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Nullable
import qualified ADL.Core.TypeToken
import qualified Data.Aeson as JS
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data CrudReqs t = CrudReqs
    { crudReqs_create :: (PostReq t T.Text)
    , crudReqs_read :: (PostReq T.Text t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCrudReqs :: (PostReq t T.Text) -> (PostReq T.Text t) -> CrudReqs t
mkCrudReqs create read = CrudReqs create read

instance (AdlValue t) => AdlValue (CrudReqs t) where
    atype _ = T.concat
        [ "test24.CrudReqs"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "create" crudReqs_create
        , genField "read" crudReqs_read
        ]
    
    jsonParser = CrudReqs
        <$> parseField "create"
        <*> parseField "read"

data PostReq i o = PostReq
    { postReq_path :: T.Text
    , postReq_reqBodyType :: (ADL.Core.TypeToken.TypeToken i)
    , postReq_respType :: (ADL.Core.TypeToken.TypeToken o)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkPostReq :: T.Text -> PostReq i o
mkPostReq path = PostReq path (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)

instance (AdlValue i, AdlValue o) => AdlValue (PostReq i o) where
    atype _ = T.concat
        [ "test24.PostReq"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy i)
        , ",", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy o)
        , ">" ]
    
    jsonGen = genObject
        [ genField "path" postReq_path
        , genField "reqBodyType" postReq_reqBodyType
        , genField "respType" postReq_respType
        ]
    
    jsonParser = PostReq
        <$> parseField "path"
        <*> parseFieldDef "reqBodyType" (ADL.Core.TypeToken.TypeToken)
        <*> parseFieldDef "respType" (ADL.Core.TypeToken.TypeToken)

data Service = Service
    { service_hello :: (PostReq T.Text T.Text)
    , service_farewell :: (PostReq (ADL.Core.Nullable.Nullable T.Text) (ADL.Core.Nullable.Nullable T.Text))
    , service_blobs :: (CrudReqs JS.Value)
    }
    deriving (Prelude.Eq,Prelude.Show)

mkService ::  Service
mkService  = Service (PostReq "/hello" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (PostReq "/farewell" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (CrudReqs (PostReq "/blobs/create" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (PostReq "/blobs/read" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)))

instance AdlValue Service where
    atype _ = "test24.Service"
    
    jsonGen = genObject
        [ genField "hello" service_hello
        , genField "farewell" service_farewell
        , genField "blobs" service_blobs
        ]
    
    jsonParser = Service
        <$> parseFieldDef "hello" (PostReq "/hello" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "farewell" (PostReq "/farewell" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "blobs" (CrudReqs (PostReq "/blobs/create" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (PostReq "/blobs/read" (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)))