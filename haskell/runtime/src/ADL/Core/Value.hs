{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Core.Value(
  JsonGen(..),
  JsonParser(..),
  ParseResult(..),
  AdlValue(..),
  StringMap(..),
  Nullable(..),

  adlToJson,
  adlFromJson,
  adlFromJsonFile,
  adlFromJsonFile',
  adlToJsonFile,
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
  parseFail,
  textFromParseContext
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
import Data.Monoid
import Data.Proxy
import Data.Int
import Data.Word

-- | A Json serialiser
newtype JsonGen a = JsonGen {runJsonGen :: a -> JS.Value}

-- | A Json parser
newtype JsonParser a = JsonParser {runJsonParser :: ParseContext -> JS.Value -> ParseResult a}

-- | A path within a json value, used in error reporting
type ParseContext = [ParseContextItem]

data ParseContextItem
  = ParseField T.Text
  | ParseItem Int

data ParseResult a
  = ParseSuccess a
  | ParseFailure T.Text ParseContext

class AdlValue a where
  -- | A text string describing the type. The return string may only depend on
  -- the type - the parameter must be ignored.
  atype :: Proxy a -> T.Text

  -- | A JSON generator for this ADL type
  jsonGen :: JsonGen a

  -- | A JSON parser for this ADL type
  jsonParser :: JsonParser a

instance Functor JsonParser where
  fmap f (JsonParser pf) = JsonParser (\ctx jv -> fmap f (pf ctx jv))

instance Applicative JsonParser where
  pure a = JsonParser (\_ _ -> ParseSuccess a)
  (JsonParser fa) <*> (JsonParser a) = JsonParser (\ctx jv -> fa ctx jv <*> a ctx jv)

instance Alternative JsonParser where
  empty = JsonParser (\_ _ -> empty)
  (JsonParser fa) <|> (JsonParser a) = JsonParser (\ctx jv -> fa ctx jv <|> a ctx jv)

instance Functor ParseResult where
  fmap f (ParseSuccess a) = ParseSuccess (f a)
  fmap _ (ParseFailure e ctx) = ParseFailure e ctx

instance Applicative ParseResult where
  pure = ParseSuccess
  (ParseFailure e ctx) <*> _ = ParseFailure e ctx
  _ <*> (ParseFailure e ctx) = ParseFailure e ctx
  (ParseSuccess a) <*> (ParseSuccess b) = ParseSuccess (a b)

instance Alternative ParseResult where
  empty = ParseFailure "" []
  ParseFailure{} <|> pr = pr
  pr <|> _ = pr

parseFail :: T.Text -> JsonParser a
parseFail t = JsonParser (\ctx _ -> (ParseFailure t ctx))

-- Convert an ADL value to a JSON value
adlToJson :: AdlValue a => a -> JS.Value
adlToJson = runJsonGen jsonGen

-- Convert a JSON value to an ADL value
adlFromJson :: AdlValue a => JS.Value -> ParseResult a
adlFromJson = runJsonParser jsonParser []

-- Write an ADL value to a JSON file.
adlToJsonFile :: AdlValue a => FilePath -> a -> IO ()
adlToJsonFile file a = LBS.writeFile file lbs
  where lbs = JS.encode (runJsonGen jsonGen a)

-- Read and parse an ADL value from a JSON file.
adlFromJsonFile :: AdlValue a => FilePath -> IO (ParseResult a)
adlFromJsonFile file = do
  lbs <- LBS.readFile file
  case JS.eitherDecode' lbs of
    (Left e) -> return (ParseFailure ("Invalid json:" <> T.pack e) [])
    (Right jv) -> return (runJsonParser jsonParser [] jv)

-- Read and parse an ADL value from a JSON file, throwing an exception
-- on failure.
adlFromJsonFile' :: forall a . AdlValue a => FilePath -> IO a
adlFromJsonFile' file = do
  ma <- adlFromJsonFile file
  case ma of
    (ParseFailure e ctx) -> ioError $ userError $
      T.unpack
        (  "Unable to parse a value of type "
        <> atype (Proxy :: Proxy a)
        <> " from " <>  T.pack file <> ": "
        <> e <> " at " <> textFromParseContext ctx
        )
    (ParseSuccess a) -> return a

textFromParseContext :: ParseContext -> T.Text
textFromParseContext [] = "$"
textFromParseContext pc = T.intercalate "." (map fmt (reverse pc))
  where
    fmt (ParseField f) = f
    fmt (ParseItem i) = "[" <> T.pack (show i) <> "]"

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
parseField label = withJsonObject $ \ctx hm -> case HM.lookup label hm of
  (Just b) -> runJsonParser jsonParser (ParseField label:ctx) b
  _ -> ParseFailure ("expected field " <> label) ctx

parseFieldDef :: AdlValue a => T.Text -> a -> JsonParser a
parseFieldDef label defv = withJsonObject $ \ctx hm -> case HM.lookup label hm of
  (Just b) -> runJsonParser jsonParser (ParseField label:ctx) b
  _ -> pure defv

parseUnionVoid :: T.Text -> a -> JsonParser a
parseUnionVoid disc a = JsonParser $ \ctx jv -> case jv of
  (JS.String s) | s == disc -> pure a
  _ -> empty

parseUnionValue :: AdlValue b => T.Text -> (b -> a) -> JsonParser a
parseUnionValue disc fa = withJsonObject $ \ctx hm -> case HM.lookup disc hm of
  (Just b) -> fa <$> runJsonParser jsonParser (ParseField disc:ctx) b
  _ -> empty

withJsonObject :: (ParseContext -> JS.Object -> ParseResult a) -> JsonParser a
withJsonObject f = JsonParser $ \ctx jv -> case jv of
  (JS.Object hm) -> f ctx hm
  _ -> ParseFailure "expected an object" ctx

withJsonNumber :: (ParseContext -> SC.Scientific -> ParseResult a) -> JsonParser a
withJsonNumber f = JsonParser $ \ctx jv -> case jv of
  (JS.Number n) -> f ctx n
  _ -> ParseFailure "expected a number" ctx

withJsonString :: (ParseContext -> T.Text -> ParseResult a) -> JsonParser a
withJsonString f = JsonParser $ \ctx jv -> case jv of
  (JS.String s) -> f ctx s
  _ -> ParseFailure "expected a string" ctx

instance AdlValue () where
  atype _ = "Void"

  jsonGen = JsonGen (const JS.Null)

  jsonParser = JsonParser $ \ctx v -> case v of
    JS.Null -> pure ()
    _ -> ParseFailure "expected null" ctx

instance AdlValue Bool where
  atype _ = "Bool"

  jsonGen = JsonGen JS.Bool

  jsonParser = JsonParser $ \ctx v -> case v of
    (JS.Bool b) -> pure b
    _ -> ParseFailure "expected a boolean" ctx

instance AdlValue Int8 where
  atype _ = "Int8"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Int16 where
  atype _ = "Int16"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Int32 where
  atype _ = "Int32"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Int64 where
  atype _ = "Int64"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Word8 where
  atype _ = "Word8"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Word16 where
  atype _ = "Word16"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Word32 where
  atype _ = "Word32"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Word64 where
  atype _ = "Word64"
  jsonGen = JsonGen (JS.Number . fromIntegral)
  jsonParser = withJsonNumber toBoundedInteger

instance AdlValue Double where
  atype _ = "Double"
  jsonGen = JsonGen (JS.Number . SC.fromFloatDigits)
  jsonParser = withJsonNumber (\_ n -> pure (SC.toRealFloat n))

instance AdlValue Float where
  atype _ = "Float"
  jsonGen = JsonGen (JS.Number . SC.fromFloatDigits)
  jsonParser = withJsonNumber (\_ n -> pure (SC.toRealFloat n))

toBoundedInteger :: forall i. (Integral i, Bounded i, AdlValue i) => ParseContext -> SC.Scientific -> ParseResult i
toBoundedInteger ctx n = case SC.toBoundedInteger n of
  Nothing -> ParseFailure ("expected an " <> atype (Proxy :: Proxy i)) ctx
  (Just i) -> pure i

instance AdlValue T.Text where
  atype _ = "String"
  jsonGen = JsonGen JS.String
  jsonParser = withJsonString (\_ s -> pure s)

instance AdlValue BS.ByteString where
  atype _ = "Bytes"
  jsonGen = JsonGen (JS.String . T.decodeUtf8 . B64.encode)
  jsonParser = withJsonString $ \ctx s -> case B64.decode (T.encodeUtf8 s) of
    Left e -> ParseFailure ("unable to decode base64 value: " <> T.pack e) ctx
    Right v -> pure v

instance forall a . (AdlValue a) => AdlValue [a] where
  atype _ = T.concat ["Vector<",atype (Proxy :: Proxy a),">"]
  jsonGen = JsonGen (JS.Array . V.fromList . (map (adlToJson)))
  jsonParser = JsonParser $ \ctx v -> case v of
    (JS.Array a) -> let parse (i,jv) = runJsonParser jsonParser (ParseItem i:ctx) jv
                    in traverse parse (zip [0,1..] (V.toList a))
    _ -> ParseFailure "expected an array" ctx

newtype StringMap v = StringMap {unStringMap :: M.Map T.Text v}
  deriving (Eq,Ord,Show)

stringMapFromList :: [(T.Text,v)] -> StringMap v
stringMapFromList = StringMap . M.fromList

instance forall a . (AdlValue a) => AdlValue (StringMap a) where
  atype _ = T.concat ["StringMap<",atype (Proxy :: Proxy a),">"]
  jsonGen = JsonGen (JS.Object . HM.fromList . (map toPair) . M.toList . unStringMap)
    where
      toPair (k,v) = (k,adlToJson v)

  jsonParser = withJsonObject $ \ctx hm ->
    let parse (k,jv) = (\jv -> (k,jv)) <$> runJsonParser jsonParser (ParseField k:ctx) jv
    in (StringMap . M.fromList) <$> traverse parse (HM.toList hm)

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

  jsonParser = JsonParser $ \ctx jv -> case jv of
    JS.Null -> pure (Nullable Nothing)
    _ -> runJsonParser jsonParser ctx jv
