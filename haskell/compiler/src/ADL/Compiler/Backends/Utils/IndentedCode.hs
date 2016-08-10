{-# LANGUAGE OverloadedStrings #-}

-- | A trivial DSL for generated indented block structured text
module ADL.Compiler.Backends.Utils.IndentedCode where

import qualified Data.Text as T

import Data.Monoid

import ADL.Utils.Format

data Code = CEmpty
          | CLine T.Text      
          | CAppend Code Code
          | CIndent Code

instance Monoid Code where
  mempty = CEmpty
  mappend = CAppend

cline :: T.Text -> Code
cline t = CLine t

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

codeText :: Int -> Code -> [T.Text]
codeText maxLineLength c = mkLines "" c
  where
    mkLines i CEmpty = []
    mkLines i (CAppend c1 c2) = mkLines i c1 <> mkLines i c2
    mkLines i (CIndent c) = mkLines (indentStr <> i) c
    mkLines i (CLine "") = [""]
    mkLines i (CLine t) = case breakLine (maxLineLength - T.length i) t of
      [] -> []
      (l1:ls) -> (i <> l1):[i <> i <> l | l <- ls]
    indentStr = "  "

breakLine :: Int -> T.Text -> [T.Text]
breakLine maxlength t
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
addTerminators first middle last ts = add0 ts
  where
    add0 (t1:t2:ts) =  (t1<>first):add1 (t2:ts)
    add0 ts = add1 ts
    add1 [t] = [t<>last]
    add1 (t:ts) = (t<>middle):add1 ts
    add1 [] = []

