{-# LANGUAGE OverloadedStrings #-}

-- | A trivial DSL for generated indented block structured text
module ADL.Utils.IndentedCode where

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L

import qualified Data.Semigroup as S
import Data.Monoid

import ADL.Utils.Format

data Code = CEmpty
          | CLine T.Text
          | CSpan Code Code
          | CAppend Code Code
          | CIndent Code
          deriving (Show)

instance S.Semigroup Code where
  (<>) = CAppend

instance Monoid Code where
  mempty = CEmpty
  mappend = (S.<>) -- redundant from ghc 8.4

cline :: T.Text -> Code
cline t = CLine t

cspan :: Code -> Code -> Code
cspan c1 c2 = CSpan c1 c2

clineN :: [T.Text] -> Code
clineN ts = mconcat (map CLine ts)

indent :: Code -> Code
indent = CIndent

indentN :: [Code] -> Code
indentN = CIndent . mconcat

cblock :: T.Text -> Code -> Code
cblock "" body = cline "{"  <> indent body <> cline "}"
cblock intro body =  cline (intro <> " {")  <> indent body <> cline "}"

cblock1 :: T.Text -> Code -> Code
cblock1 intro body =
  cline (intro <> " {")  <> indent body <> cline "};"

coverride :: T.Text -> Code -> Code
coverride intro body =
  cline "@Override" <> cline (intro <> " {")  <> indent body <> cline "}"

ctemplate :: T.Text -> [T.Text] -> Code
ctemplate pattern params = cline $ template pattern params

codeText :: Maybe Int -> Code -> [T.Text]
codeText maxLineLength c = mkLines "" c
  where
    mkLines i CEmpty = []
    mkLines i (CAppend c1 c2) = mkLines i c1 <> mkLines i c2
    mkLines i (CSpan c1 c2) = joinSpannedText i (mkLines i c1) (mkLines i c2)
    mkLines i (CIndent c) = mkLines (indentStr <> i) c
    mkLines i (CLine "") = [""]
    mkLines i (CLine t) = case breakLine ((-) <$> maxLineLength <*> (pure (T.length i))) t of
      [] -> []
      (l1:ls) -> (i <> l1):[i <> i <> l | l <- ls]
    indentStr = "  "

joinSpannedText :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
joinSpannedText _ [] [] = []
joinSpannedText _ list1 [] = list1
joinSpannedText _ [] list2 = list2
joinSpannedText prefix list1 list2 =
  L.init list1 <>  [joinedLine] <> L.tail list2
  where
    joinedLine = (L.last list1) <> lines2HeadUnindented
    (_, lines2HeadUnindented) = T.splitAt (T.length prefix) list2Head
    list2Head = L.head list2

breakLine :: Maybe Int -> T.Text -> [T.Text]
breakLine Nothing t = [t]
breakLine (Just maxlength) t
  | T.length t < maxlength = [t]
  | otherwise = map (T.strip . T.concat) (assemble 0 [] (lineBreakChunks t))
  where
    assemble len cline [] = [cline]
    assemble len cline (c:cs)
      | len + T.length c > maxlength = case cline of
           [] -> assemble (T.length c) [c] cs
           _ -> cline : assemble (T.length c) [c] cs
      | otherwise = assemble (len + T.length c) (cline++[c]) cs


lineBreakChunks :: T.Text -> [T.Text]
lineBreakChunks t = map (T.pack . reverse) (chunks "" (T.unpack t))
  where
    -- We break after ',' '=', or '(', but never within
    -- strings
    chunks cs [] = [cs]
    chunks cs ('\'':s) = quote1 ('\'':cs) s
    chunks cs ('\"':s) = quote2 ('\"':cs) s
    chunks cs (',':s) = (',':cs) : chunks "" s
    chunks cs ('=':s) = ('=':cs) : chunks "" s
    chunks cs ('(':s) = ('(':cs) : chunks "" s
    chunks cs (c:s) =  chunks (c:cs) s

    quote1 cs [] = [cs]
    quote1 cs ('\'':s) = chunks ('\'':cs) s
    quote1 cs ('\\':'\'':s) = quote1 ('\\':'\'':cs) s
    quote1 cs (c:s) =  quote1 (c:cs) s

    quote2 cs [] = [cs]
    quote2 cs ('\"':s) = chunks ('\"':cs) s
    quote2 cs ('\\':'\"':s) = quote2 ('\\':'\"':cs) s
    quote2 cs (c:s) =  quote2 (c:cs) s

-- | Add appropriate  terminators to each of the text values in the
-- given list
addTerminators :: T.Text -> T.Text -> T.Text -> [T.Text] -> [T.Text]
addTerminators first other last ts = map (\(term,t) -> t<>term) (edgeCases first other last ts)

-- | Assign first,other,last values to items in a list
edgeCases :: a -> a -> a -> [b] -> [(a,b)]
edgeCases first other last bs = add0 bs
  where
    add0 (b1:b2:bs) =  (first,b1):add1 (b2:bs)
    add0 bs = add1 bs
    add1 [b] = [(last,b)]
    add1 (b:bs) = ((other,b)):add1 bs
    add1 [] = []

capitalise :: T.Text -> T.Text
capitalise text = T.cons (C.toUpper (T.head text)) (T.tail text)

