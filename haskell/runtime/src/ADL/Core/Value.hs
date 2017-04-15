{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Value(
  JsonGen(..),
  JsonParser(..),
  AdlValue(..),
  StringMap(..),
  Nullable(..),

  adlToJson,
  adlFromJson,
  aFromJSONFile,
  aFromJSONFile',
  aToJSONFile,
  genField,
  genObject,
  genUnion,
  genUnionValue,
  genUnionVoid,
  parseField,
  parseFieldDef,
  parseUnionValue,
  parseUnionVoid,
  stringMapFromList,
) where

import qualified Data.Aeson as JS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Scientific as SC
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Control.Applicative
import Data.Proxy
import Data.Int
import Data.Word

-- | A Json serialiser
newtype JsonGen a = JsonGen {runJsonGen :: a -> JS.Value}

-- | A Json parser
newtype JsonParser a = JsonParser {runJsonParser :: JS.Value -> Maybe a}

class AdlValue a where
  -- | A text string describing the type. The return string may only depend on
  -- the type - the parameter must be ignored.
  atype :: Proxy a -> T.Text

  -- | A JSON generator for this ADL type
  jsonGen :: JsonGen a

  -- | A JSON parser for this ADL type
  jsonParser :: JsonParser a

instance Functor JsonParser where
  fmap f (JsonParser pf) = JsonParser (fmap (fmap f) pf)

instance Applicative JsonParser where
  pure = JsonParser . const . Just
  (JsonParser fa) <*> (JsonParser a) = JsonParser (\jv -> fa jv <*> a jv)

instance Alternative JsonParser where
  empty = JsonParser (const empty)
  (JsonParser fa) <|> (JsonParser a) = JsonParser (\jv -> fa jv <|> a jv)


-- Convert an ADL value to a JSON value
adlToJson :: AdlValue a => a -> JS.Value
adlToJson = runJsonGen jsonGen

-- Convert a JSON value to an ADL value
adlFromJson :: AdlValue a => JS.Value -> Maybe a
adlFromJson = runJsonParser jsonParser

-- Write an ADL value to a JSON file.
aToJSONFile :: JsonGen a -> FilePath -> a -> IO ()
aToJSONFile jg file a = LBS.writeFile file lbs
  where lbs = JS.encode (runJsonGen jg a)

-- Read and parse an ADL value from a JSON file. 
aFromJSONFile :: JsonParser a -> FilePath -> IO (Maybe a)
aFromJSONFile jp file = do
  lbs <- LBS.readFile file
  case JS.eitherDecode' lbs of
    (Left _) -> return Nothing
    (Right jv) -> return (runJsonParser jp jv)

-- Read and parse an ADL value from a JSON file, throwing an exception
-- on failure.    
aFromJSONFile' :: forall a .(AdlValue a) => JsonParser a -> FilePath -> IO a
aFromJSONFile' jg file = do
  ma <- aFromJSONFile jg file
  case ma of
    Nothing -> ioError $ userError
      ("Unable to parse a value of type " ++
       T.unpack (atype (Proxy :: Proxy a)) ++ " from " ++ file)
    (Just a) -> return a

genObject :: [o -> (T.Text, JS.Value)] -> JsonGen o
genObject fieldfns = JsonGen (\o -> JS.object [f o | f <- fieldfns])

genField :: AdlValue a => T.Text -> (o -> a) -> o -> (T.Text, JS.Value)
genField label f o = (label,adlToJson (f o))

genUnion :: (u -> JS.Value) -> JsonGen u
genUnion f = JsonGen f
  
genUnionValue :: AdlValue a => T.Text -> a -> JS.Value
genUnionValue disc a = JS.object [(disc,adlToJson a)]

genUnionVoid :: T.Text -> JS.Value
genUnionVoid disc = JS.toJSON disc
 
parseField :: AdlValue a => T.Text -> JsonParser a
parseField label = withJsonObject $ \hm -> case HM.lookup label hm of
  (Just b) -> runJsonParser jsonParser b
  _ -> empty

parseFieldDef :: AdlValue a => T.Text -> a -> JsonParser a
parseFieldDef label defv = withJsonObject $ \hm -> case HM.lookup label hm of
  (Just b) -> runJsonParser jsonParser b
  _ -> pure defv

parseUnionVoid :: T.Text -> a -> JsonParser a
parseUnionVoid disc a = JsonParser $ \jv -> case jv of
  (JS.String s) | s == disc -> pure a
  _ -> empty

parseUnionValue :: AdlValue b => T.Text -> (b -> a) -> JsonParser a
parseUnionValue disc fa = withJsonObject $ \hm -> case HM.lookup disc hm of
  (Just b) -> fa <$> runJsonParser jsonParser b
  _ -> empty

withJsonObject :: (JS.Object -> Maybe a) -> JsonParser a
withJsonObject f = JsonParser $ \jv -> case jv of
  (JS.Object hm) -> f hm
  _ -> Nothing

withJsonNumber :: (SC.Scientific -> Maybe a) -> JsonParser a
withJsonNumber f = JsonParser $ \jv -> case jv of
  (JS.Number n) -> f n
  _ -> Nothing

withJsonString :: (T.Text -> Maybe a) -> JsonParser a
withJsonString f = JsonParser $ \jv -> case jv of
  (JS.String s) -> f s
  _ -> Nothing

instance AdlValue () where
  atype _ = "Void"

  jsonGen = JsonGen (const JS.Null)
  
  jsonParser = JsonParser $ \v -> case v of
    JS.Null -> Just ()
    _ -> Nothing

instance AdlValue Bool where
  atype _ = "Bool"

  jsonGen = JsonGen JS.Bool
  
  jsonParser = JsonParser $ \v -> case v of
    (JS.Bool b) -> Just b
    _ -> Nothing

instance AdlValue Int8 where
  atype _ = "Int8"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Int16 where
  atype _ = "Int16"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Int32 where
  atype _ = "Int32"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Int64 where
  atype _ = "Int64"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Word8 where
  atype _ = "Word8"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Word16 where
  atype _ = "Word16"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Word32 where
  atype _ = "Word32"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Word64 where
  atype _ = "Word64"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber SC.toBoundedInteger

instance AdlValue Double where
  atype _ = "Double"
  jsonGen = JsonGen (JS.Number . SC.fromFloatDigits)
  jsonParser = withJsonNumber (Just . SC.toRealFloat)

instance AdlValue Float where
  atype _ = "Float"
  jsonGen = JsonGen (JS.Number . SC.fromFloatDigits)
  jsonParser = withJsonNumber (Just . SC.toRealFloat)

instance AdlValue T.Text where
  atype _ = "String"
  jsonGen = JsonGen JS.String
  jsonParser = withJsonString (Just . id)

instance AdlValue BS.ByteString where
  atype _ = "Bytes"
  jsonGen = JsonGen (JS.String . T.decodeUtf8 . B64.encode)
  jsonParser = withJsonString (either (const Nothing) Just . B64.decode . T.encodeUtf8)

instance forall a . (AdlValue a) => AdlValue [a] where
  atype _ = T.concat ["Vector<",atype (Proxy :: Proxy a),">"]
  jsonGen = JsonGen (JS.Array . V.fromList . (map (adlToJson)))
  jsonParser = JsonParser $ \v -> case v of
    (JS.Array a) -> mapM (runJsonParser jsonParser) (V.toList a)
    _ -> Nothing
 
newtype StringMap v = StringMap {unStringMap :: M.Map T.Text v}
  deriving (Eq,Ord,Show)

stringMapFromList :: [(T.Text,v)] -> StringMap v
stringMapFromList = StringMap . M.fromList

instance forall a . (AdlValue a) => AdlValue (StringMap a) where
  atype _ = T.concat ["StringMap<",atype (Proxy :: Proxy a),">"]
  jsonGen = JsonGen (JS.Object . HM.fromList . (map toPair) . M.toList . unStringMap)
    where
      toPair (k,v) = (k,adlToJson v)

  jsonParser = JsonParser $ \v -> case v of
    (JS.Object hm) -> (StringMap . M.fromList) <$> mapM fromPair (HM.toList hm)
    _ -> Nothing
    where
      fromPair (k,jv) = do
        v <- runJsonParser jsonParser jv
        return (k,v)

instance (AdlValue t) => AdlValue (Maybe t) where
  atype _ = T.concat
    [ "sys.types.Maybe"
    , "<", atype (Proxy :: Proxy t)
    , ">" ]

  jsonGen = genUnion $ \v -> case v of
    Nothing -> genUnionVoid "nothing"
    (Just v1) -> genUnionValue "just" v1

  jsonParser
    =   parseUnionVoid "nothing" Nothing
    <|> parseUnionValue "just" Just

instance (AdlValue t1, AdlValue t2) => AdlValue (Either t1 t2) where
  atype _ = T.concat
        [ "sys.types.Either"
        , "<", atype (Proxy :: Proxy t1)
        , ",", atype (Proxy :: Proxy t2)
        , ">" ]
    
  jsonGen = genUnion  $ \v -> case v of
    (Left v1) -> genUnionValue "left" v1
    (Right v2) -> genUnionValue "right" v2

  jsonParser
    =   parseUnionValue "left" Left
    <|> parseUnionValue "right" Right

instance forall t1 t2 . (AdlValue t1, AdlValue t2) => AdlValue (t1,t2) where
  atype _ = T.concat
        [ "sys.types.Pair"
        , "<", atype (Proxy :: Proxy t1)
        , ",", atype (Proxy :: Proxy t2)
        , ">" ]
    
  jsonGen = genObject
    [ genField "v1" fst
    , genField "v2" snd
    ]

  jsonParser = (,)
    <$> parseField "v1"
    <*> parseField "v2"

instance (AdlValue k, Ord k, AdlValue v) => AdlValue (M.Map k v) where
  atype _ = atype (Proxy :: Proxy [(k,v)])
  jsonGen = JsonGen (adlToJson . M.toList)
  jsonParser = M.fromList <$> jsonParser

instance (Ord v, AdlValue v) => AdlValue (S.Set v) where
  atype _ = atype (Proxy :: Proxy [v])
  jsonGen = JsonGen (adlToJson . S.toList)
  jsonParser = S.fromList <$> jsonParser

newtype Nullable t = Nullable (Maybe t)
  deriving (Eq,Ord,Show)

instance (AdlValue t) => AdlValue (Nullable t) where
  atype _ = T.concat
        [ "sys.types.Nullable"
        , "<", atype (Proxy :: Proxy t)
        , ">" ]
  
  jsonGen = JsonGen $ \v -> case v of
    (Nullable Nothing) -> JS.Null
    (Nullable (Just v1)) -> adlToJson v1

  jsonParser = JsonParser $ \jv -> case jv of
    JS.Null -> return (Nullable Nothing)
    _ -> Nullable <$> adlFromJson jv
