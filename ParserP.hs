module Parser where

import Data.Char

import Data.Char
import Data.Maybe
import Control.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Combinator as P

import AST

whiteSpace :: P.Parser ()
whiteSpace
  =   P.space *> whiteSpace
  <|> P.try (P.string "//" *> P.many (P.noneOf "\n") *> whiteSpace)
  <|> pure ()

ident0 :: P.Parser Ident
ident0 = T.cons <$> P.letter
                <*> (T.pack <$> P.many (P.alphaNum <|> P.char '_'))

ctoken :: Char -> P.Parser T.Text
ctoken c = (T.singleton <$> P.char c) <* whiteSpace

token :: String -> P.Parser T.Text
token s = (T.pack <$> P.try (P.string s)) <* whiteSpace

name :: P.Parser Ident
name = ident0 <* whiteSpace

moduleName :: P.Parser ModuleName
moduleName = P.sepBy1 ident0 (P.char '.') <* whiteSpace

scopedName :: P.Parser ScopedName
scopedName = (\is -> ScopedName (init is) (last is))  <$> moduleName

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
      Nothing -> return (TE_Ref n)
      (Just ps) -> return (TE_Apply n ps)

field :: P.Parser (Field ScopedName)
field = do
    t <- typeExpression
    n <- name
    return (Field n t Nothing Map.empty)

struct :: P.Parser (Ident,Struct ScopedName)
struct = do
    token "struct"
    n <- name
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,Struct tparams fields)

union :: P.Parser (Ident,Union ScopedName)
union = do
    token "union"
    n <- name
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,Union tparams fields)

typedef :: P.Parser (Ident,Typedef ScopedName)
typedef = do
    token "typedef"
    te <- typeExpression
    n <- name
    tparams <- optTypeParamList
    return (n,Typedef tparams te)

decl :: P.Parser (Decl ScopedName)
decl =   (mkStruct <$> struct)
     <|> (mkUnion <$> union)
     <|> (mkTypedef <$> typedef)
  where
    mkStruct (n,s) = Decl n Map.empty (Decl_Struct s)
    mkUnion (n,u) = Decl n Map.empty (Decl_Union u)
    mkTypedef (n,t) = Decl n Map.empty (Decl_Typedef t)

importP :: P.Parser ModuleName
importP = token "import" *> moduleName

moduleP :: P.Parser (Module ScopedName)
moduleP = do
    token "module"
    name <- moduleName
    ctoken '{'
    imports <- P.many (importP <* ctoken ';')
    decls <- P.many (decl <* ctoken ';')
    ctoken '}'
    return (Module name imports decls)


module2 :: P.Parser (Module ScopedName)
module2 = token "module" *> (
    Module <$> ( moduleName <* ctoken '{' )
           <*> P.many (importP <* ctoken ';')
           <*> P.many (decl <* ctoken ';')
    ) <* ctoken '}'

moduleFile :: P.Parser (Module ScopedName)
moduleFile = whiteSpace *> moduleP <* ctoken ';'
    

fromFile :: P.Parser a -> FilePath -> IO (Either P.ParseError a)
fromFile p fpath = do
    bs <- BS.readFile fpath
    return (P.parse p fpath (T.decodeUtf8 bs))

fromString :: P.Parser a -> FilePath -> Either P.ParseError a
fromString p s = P.parse p "string" (T.pack s)

