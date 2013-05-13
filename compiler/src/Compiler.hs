{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import System.FilePath(joinPath)
import Data.List(intercalate,partition)
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text as T
import qualified Data.Text.IO as T

import EIO
import ADL.Utils.Format
import AST
import Processing
import qualified Backends.Haskell as H

type EIOT a = EIO T.Text a

loadAndCheckModule :: ModuleFinder -> FilePath -> EIOT RModule
loadAndCheckModule moduleFinder modulePath = do
    (m,mm) <- loadModule1 
    mapM_ checkModuleForDuplicates (Map.elems mm)
    checkModuleForDuplicates m
    let mmSorted = sortByDeps (Map.elems mm)
    ns <- resolveN mmSorted emptyNameScope
    (_,rm) <- resolve1 m ns
    return rm

  where
    loadModule1 :: EIOT (SModule, SModuleMap)
    loadModule1 = loadModule modulePath moduleFinder Map.empty

    checkModuleForDuplicates :: SModule -> EIOT ()
    checkModuleForDuplicates m = case dups of
        [] -> return ()
        _ -> eioError (moduleErrorMessage m (map formatText dups))
      where
        dups = checkDuplicates m

    resolveN :: [SModule] -> NameScope -> EIOT NameScope
    resolveN [] ns = return ns
    resolveN (m:ms) ns = do
        (ns',rm) <- resolve1 m ns
        resolveN ms ns'
    
    resolve1 :: SModule -> NameScope -> EIOT (NameScope,RModule)
    resolve1 m ns = do
        liftIO $ putStrLn ("processing " ++ format (m_name m) ++ "...")
        checkUndefined1 m ns
        let rm = resolveModule m ns
            mdecls = Map.mapKeys (\i -> ScopedName (m_name rm) i) (m_decls rm)
            ns' = ns{ns_globals=Map.union (ns_globals ns) mdecls}
        checkTypeCtorApps1 rm
        return (ns', rm)

    checkUndefined1 :: SModule -> NameScope -> EIOT ()
    checkUndefined1 m ns = case undefinedNames m ns of
        [] -> return ()
        udefs -> eioError (moduleErrorMessage m (map formatText udefs))

    checkTypeCtorApps1 :: RModule -> EIOT ()
    checkTypeCtorApps1 m = case checkTypeCtorApps m of
        [] -> return ()      
        errs -> eioError (moduleErrorMessage m (map formatText errs))

    moduleErrorMessage m ss = T.intercalate " " ["In module",formatText(m_name m),":\n"] `T.append`
                              T.intercalate "\n  " ss
          
    emptyNameScope = NameScope Map.empty Map.empty Map.empty Set.empty

sortByDeps :: [SModule] -> [SModule]
sortByDeps ms = map fst (sort0 (modulesWithDeps ms) Set.empty)
  where
    sort0 :: [(SModule,Set.Set ModuleName)] -> Set.Set ModuleName -> [(SModule,Set.Set ModuleName)]
    sort0 [] _ = []
    sort0 ms sofar = let (ok,todo) = partition (\(m,md)-> Set.null (md `Set.difference` sofar)) ms
                         sofar' = Set.unions (sofar:map (Set.singleton . m_name . fst) ok)
                     in ok ++ sort0 todo sofar'
                         
    modulesWithDeps ms = map (\m -> (m,getReferencedModules m)) ms

type ModuleFinder = ModuleName -> [FilePath]

moduleFinder :: [FilePath] -> ModuleFinder
moduleFinder rootpaths (ModuleName mname) =
    [ joinPath ([path]++names++[name0++".adl"]) | path <- rootpaths ]
  where
    names = map T.unpack (init mname)
    name0 = T.unpack (last mname)

data VerifyFlags = VerifyFlags {
  vf_searchPath :: [FilePath]
}

verify :: VerifyFlags -> [FilePath] -> EIO T.Text ()
verify vf modulePaths = catchIOError $ forM_ modulePaths $ \modulePath -> do
  loadAndCheckModule (moduleFinder (vf_searchPath vf)) modulePath

data HaskellFlags = HaskellFlags {
  hf_searchPath :: [FilePath],
  hf_modulePrefix :: String,
  hf_outputPath :: FilePath,
  hf_noOverwrite :: Bool
}

haskell :: HaskellFlags -> [FilePath] -> EIO T.Text ()
haskell hf modulePaths = catchIOError $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule (moduleFinder (hf_searchPath hf)) modulePath
  H.writeModuleFile (hf_noOverwrite hf)
                    (H.moduleMapper (hf_modulePrefix hf))
                    (H.fileMapper (hf_outputPath hf)) rm

catchIOError :: EIO T.Text a -> EIO T.Text a
catchIOError a = eioHandle handler a
 where
   handler :: IOError -> EIO T.Text a
   handler e = eioError (T.pack (show e))