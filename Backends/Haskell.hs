{-# LANGUAGE OverloadedStrings #-}
module Backends.Haskell where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Format
import AST
import Processing

data MState = MState {
   ms_indent :: Int,
   ms_lines :: [T.Text]
}

type HGState = Map.Map ModuleName MState
type HaskellGenerator = State HGState

updateMState :: ModuleName -> (MState->MState) -> HaskellGenerator ()
updateMState mn uf = do
  modify $ \s -> case Map.lookup mn s of
    Nothing -> Map.insert mn (uf (MState 0 [])) s
    (Just ms) -> Map.insert mn (uf ms) s

writeLine :: ModuleName -> T.Text -> HaskellGenerator ()
writeLine mn t = updateMState mn (\ms -> ms{ms_lines=t:ms_lines ms})

-- | `template src substs` will replace all occurences the string $i
-- in src with `substs !! i`
template :: T.Text -> [T.Text] -> T.Text
template t substs = foldr replace t (zip [1,2..] substs)
  where
    replace (i,s) t = T.replace (T.pack ('$':show i)) s t

withIndent :: ModuleName -> HaskellGenerator a -> HaskellGenerator a
withIndent m g = do
    updateMState m (\ms -> ms{ms_indent=ms_indent ms + 4})
    a <- g
    updateMState m (\ms -> ms{ms_indent=ms_indent ms - 4})
    return a

helpers mn = ( writeLine mn
             , \t ss -> writeLine mn (template t ss)
             , withIndent mn
             )

upper1,lower1 :: T.Text -> T.Text
upper1 t = T.toUpper (T.pack [(T.head t)]) `T.append` T.tail t
lower1 t = T.toLower (T.pack [(T.head t)]) `T.append` T.tail t

hTypeName :: Ident -> Ident
hTypeName sn = upper1 sn

hFieldName :: Ident -> Ident -> Ident
hFieldName sn fn = T.concat [lower1 sn,"_",fn]

hDiscName :: Ident -> Ident -> Ident
hDiscName sn fn = T.concat [upper1 sn,"_",fn]

generate :: ModuleName -> Decl ResolvedType -> HaskellGenerator ()
generate mn d@(Decl{d_type=(Decl_Struct s)}) = do
    let (wl,wt,indent) = helpers mn
        sname = hTypeName (d_name d)

    wt "data $1 = $1 {" [sname]
    indent $ do
        forM_ (s_fields s) $ \f -> do
           wt "$1 ::" [hFieldName (d_name d) (f_name f)]
        wl "}"
    wl ""
    wt "instance DefaultV $1 where" [sname]
    indent $ do
        wl "defaultv = undefined"
    wl ""
    wt "instance WriteJSON $1 where" [sname]
    indent $ do
        wl "writeJSON = undefined"
    wl ""
    wt "instance ReadJSON $1 where" [sname]
    indent $ do
        wl "readJSON = undefined"
    wl ""

generateHaskell rm =
      liftIO $ mapM (T.putStrLn . formatText . d_name)  (Map.elems $ m_decls rm)

