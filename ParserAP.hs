{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char
import Data.Maybe
import Control.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as BS

import AST

ident0 :: A.Parser Ident
ident0 = T.cons <$> A.letter <*> A.takeWhile isAlphaNum
  where
    isAlphaNum c = isAlpha c || isDigit c || c == '_'

token :: String -> A.Parser T.Text
token s = (A.skipSpace *> A.string (T.pack s)) A.<?> ("token " ++ s)

name :: A.Parser Ident
name = A.skipSpace *> ident0 A.<?> "name"

moduleName :: A.Parser ModuleName
moduleName = A.skipSpace *> A.sepBy1 ident0 (A.char '.') A.<?> "moduleName"

scopedName :: A.Parser ScopedName
scopedName = fmap (\is -> ScopedName (init is) (last is) )  moduleName A.<?> "ScopedName"

angleList :: A.Parser a -> A.Parser [a]
angleList p = token "<" *> A.sepBy1 p (token ",") <* token ">"

parenList :: A.Parser a -> A.Parser [a]
parenList p = token "{" *> many (p <* token ";") <* token "}"

optTypeParamList :: A.Parser [Ident]
optTypeParamList = fromMaybe [] <$> optional (angleList name)

typeExpression :: A.Parser (TypeExpr ScopedName)
typeExpression =
    TE_Apply <$> scopedName <*> angleList typeExpression
    <|>
    TE_Ref <$> scopedName

field :: A.Parser (Field ScopedName)
field = do
    t <- typeExpression
    n <- name
    token ";"
    return (Field n t Nothing Map.empty)

struct :: A.Parser (Ident,Struct ScopedName)
struct = do
    token "struct"
    n <- name
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,Struct tparams fields)

union :: A.Parser (Ident,Union ScopedName)
union = do
    token "union"
    n <- name
    tparams <- optTypeParamList
    fields <- parenList field
    return (n,Union tparams fields)

typedef :: A.Parser (Ident,Typedef ScopedName)
typedef = do
    token "typedef"
    te <- typeExpression
    n <- name
    tparams <- optTypeParamList
    return (n,Typedef tparams te)

decl :: A.Parser (Decl ScopedName)
decl =   mkStruct <$> struct
     <|> mkUnion <$> union
     <|> mkTypedef <$> typedef
  where
    mkStruct (n,s) = Decl n Map.empty (Decl_Struct s)
    mkUnion (n,u) = Decl n Map.empty (Decl_Union u)
    mkTypedef (n,t) = Decl n Map.empty (Decl_Typedef t)

moduleP :: A.Parser (Module ScopedName)
moduleP = do
    token "module"
    n <- moduleName
    decls <- parenList decl
    return (Module n decls)

test :: A.Parser (Module ScopedName)
test = do
    token "module"
    n <- moduleName
    token "{"
    token "struct"
    token "}"
    return (Module n [] )

moduleFile :: A.Parser (Module ScopedName)
moduleFile = do
    m <- moduleP
    A.skipSpace
    A.endOfInput
    return m

fromFile :: A.Parser a -> FilePath -> IO (Either String a)
fromFile p fpath = do
    bs <- BS.readFile fpath
    return (A.parseOnly p (T.decodeUtf8 bs))
