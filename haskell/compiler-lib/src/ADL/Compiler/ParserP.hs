{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.ParserP where

import Data.Char

import Data.Char
import Data.Maybe
import Numeric(readHex)
import Control.Applicative

import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.Number as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Combinator as P

import ADL.Compiler.AST

whiteSpace :: P.Parser ()
whiteSpace
  =   P.space *> whiteSpace
  <|> P.try (P.string "//" *> P.many (P.noneOf "\n") *> whiteSpace)
  <|> pure ()

ident0 :: P.Parser Ident
ident0 = do
  i <- T.cons <$> P.letter <*> (T.pack <$> P.many (P.alphaNum <|> P.char '_'))
  case isReserved i of
    True -> P.unexpected ("reserved word '"++ T.unpack i ++ "'")
    False -> return i
  where
    isReserved i = any (==i) ["module","import","struct","union","type","newtype"]

ctoken :: Char -> P.Parser T.Text
ctoken c = (T.singleton <$> P.char c) <* whiteSpace

token :: String -> P.Parser T.Text
token s = (T.pack <$> P.try (P.string s)) <* whiteSpace

name :: P.Parser Ident
name = ident0 <* whiteSpace

moduleName :: P.Parser ModuleName
moduleName = ModuleName <$> P.sepBy1 ident0 (P.char '.') <* whiteSpace

scopedName :: P.Parser ScopedName
scopedName = (\(ModuleName is) -> ScopedName (ModuleName (init is)) (last is))  <$> moduleName

angleList :: P.Parser a -> P.Parser [a]
angleList p = P.between (ctoken '<') (ctoken '>') (P.sepBy p (ctoken ','))

parenList :: P.Parser a -> P.Parser [a]
parenList p =  P.between (ctoken '{') (ctoken '}') (P.many (p <* ctoken ';'))

optTypeParamList :: P.Parser [Ident]
optTypeParamList = P.option [] (angleList name)

typeExpression :: P.Parser (TypeExpr ScopedName)
typeExpression = do
    n <- scopedName
    mps <- P.optionMaybe (angleList typeExpression)
    case mps of
      Nothing -> return (TypeExpr n [])
      (Just ps) -> return (TypeExpr n ps)

field :: P.Parser (Field ScopedName)
field = do
    t <- typeExpression
    n <- name
    mdefault <- P.optionMaybe (ctoken '=' *> jsonValue)
    return (Field n t mdefault Map.empty)

mversion :: P.Parser MVersion
mversion =   (Just . fromIntegral) <$> (ctoken '#' *> parseInt)
         <|> return Nothing

struct :: P.Parser (Ident,MVersion,Struct ScopedName)
struct = do
    token "struct"
    n <- name
    mv <- mversion
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,mv,Struct tparams fields)

union :: P.Parser (Ident,MVersion,Union ScopedName)
union = do
    token "union"
    n <- name
    mv <- mversion
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,mv,Union tparams fields)

type_ :: P.Parser (Ident,MVersion,Typedef ScopedName)
type_ = do
    token "type"
    n <- name
    mv <- mversion
    tparams <- optTypeParamList
    ctoken '='
    te <- typeExpression
    return (n,mv,Typedef tparams te)

newtype_ :: P.Parser (Ident,MVersion,Newtype ScopedName)
newtype_ = do
    token "newtype"
    n <- name
    mv <- mversion
    tparams <- optTypeParamList
    token "="
    te <- typeExpression
    mdefault <- P.optionMaybe (ctoken '=' *> jsonValue)
    return (n,mv,Newtype tparams te mdefault)

decl :: P.Parser (Decl ScopedName)
decl =   (mkStruct <$> struct)
     <|> (mkUnion <$> union)
     <|> (mkType <$> type_)
     <|> (mkNewtype <$> newtype_)
  where
    mkStruct (n,mv,s) = Decl n mv Map.empty (Decl_Struct s)
    mkUnion (n,mv,u) = Decl n mv Map.empty (Decl_Union u)
    mkType (n,mv,t) = Decl n mv Map.empty (Decl_Typedef t)
    mkNewtype (n,mv,nt) = Decl n mv Map.empty (Decl_Newtype nt)

importP :: P.Parser Import
importP = token "import" *> (P.try importAll <|> import1 )
  where
    importAll = (Import_Module . ModuleName) <$> star
      where
        star = (:) <$> ident0 <*> star2 
        star2 = P.char '.' *> ((P.char '*' *> pure []) <|> star)

    import1  = Import_ScopedName <$> scopedName

moduleP :: P.Parser (Module0 ScopedName)
moduleP = token "module" *> (
    Module0 <$> ( moduleName <* ctoken '{' )
            <*> P.many (importP <* ctoken ';')
            <*> P.many (decl <* ctoken ';')
    ) <* ctoken '}'

----------------------------------------------------------------------
jsonValue :: P.Parser JSON.Value
jsonValue =   p_null <|> p_true <|> p_false <|> p_string <|> p_int <|> p_double
          <|> p_array <|> p_object
  where
    p_null = JSON.Null <$ token "null"
    p_true = JSON.Bool True <$ token "true"
    p_false = JSON.Bool False <$ token "false"
    p_string = JSON.String <$> p_string0

    p_string0 :: P.Parser T.Text           
    p_string0 = T.pack <$> (P.char '"' *> P.many p_char <* P.char '"' <* whiteSpace)

    p_char :: P.Parser Char
    p_char = (P.char '\\' >> p_echar) <|> (P.satisfy (\x -> x /= '"' && x /= '\\'))
    p_echar =  ('"' <$ P.char '"')
          <|> ('\\' <$ P.char '\\')
          <|> ('/'  <$ P.char '/')
          <|> ('\b' <$ P.char 'b')
          <|> ('\f' <$ P.char 'f')
          <|> ('\n' <$ P.char 'n')
          <|> ('\r' <$ P.char 'r')
          <|> ('\t' <$ P.char 't')
          <|> (P.char 'u' *> p_uni)
          P.<?> "escape character"
    p_uni = check =<< P.count 4 P.hexDigit
      where check x | code <= max_char  = pure (toEnum code)
                    | otherwise         = empty
              where code      = fst $ head $ readHex x
                    max_char  = fromEnum (maxBound :: Char)

    p_int = JSON.Number . JSON.I <$> parseInt

    p_double = JSON.Number . JSON.D <$> pread reads <* whiteSpace P.<?> "number"

    p_array = JSON.Array . V.fromList <$>
              (ctoken '[' *> P.sepBy jsonValue (ctoken ',') <* ctoken ']')

    p_object = JSON.Object . HM.fromList <$>
               (ctoken '{' *> P.sepBy p_field (ctoken ',') <* ctoken '}')

    p_field :: P.Parser (T.Text,JSON.Value)
    p_field = (,) <$> (p_string0 <* ctoken ':') <*> jsonValue

pread :: ReadS a -> P.Parser a
pread reads = do
  s <- P.getInput
  case reads (T.unpack s) of
     [(v,s')] -> (v <$ P.setInput (T.pack s'))
     _ -> empty

parseInt = pread reads <* whiteSpace P.<?> "number"

----------------------------------------------------------------------


moduleFile :: P.Parser (Module0 ScopedName)
moduleFile = whiteSpace *> moduleP <* ctoken ';'
    

fromFile :: P.Parser a -> FilePath -> IO (Either P.ParseError a)
fromFile p fpath = do
    bs <- BS.readFile fpath
    return (P.parse p fpath (T.decodeUtf8 bs))

fromString :: P.Parser a -> FilePath -> Either P.ParseError a
fromString p s = P.parse p "string" (T.pack s)

