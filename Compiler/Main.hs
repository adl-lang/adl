{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)
import Control.Monad.Trans
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text as T
import qualified Data.Text.IO as T

import EIO
import Format
import AST
import Processing
import qualified Backends.Haskell as H

type EIOT a = EIO T.Text a

loadAndCheckModule :: ModuleFinder -> FilePath -> EIOT RModule
loadAndCheckModule moduleFinder modulePath = do
    (m,mm) <- loadModule1 
    mapM_ checkModuleForDuplicates (Map.elems mm)
    checkModuleForDuplicates m
    ns <- resolveN (sortByDeps (Map.elems mm)) emptyNameScope
    (_,rm) <- resolve1 m ns
    return rm

  where
    loadModule1 :: EIOT (SModule, SModuleMap)
    loadModule1 = mapError (T.pack . show) $ loadModule modulePath moduleFinder Map.empty

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
                     in ok ++ sort0 todo (Set.unions (sofar:map snd ok))
                         
    modulesWithDeps ms = map (\m -> (m,getReferencedModules m)) ms

type ModuleFinder = ModuleName -> [FilePath]

moduleFinder :: [FilePath] -> ModuleFinder
moduleFinder rootpaths (ModuleName mname) =
    [ joinPath ([path]++names++[name0++".adl"]) | path <- rootpaths ]
  where
    names = map T.unpack (init mname)
    name0 = T.unpack (last mname)

runEIO :: EIOT a -> IO ()
runEIO eio = do
    e <- unEIO $ eio
    case e of
        (Left perr) -> T.putStrLn perr
        (Right _) -> return ()
      
verify searchPaths modulePath =
    runEIO $ loadAndCheckModule (moduleFinder searchPaths) modulePath 

haskell searchPaths modulePath = runEIO $ do
    rm <- loadAndCheckModule (moduleFinder searchPaths) modulePath
    H.writeModuleFile H.moduleMapper H.fileMapper rm

usage = do
    putStrLn "Usage: adl verify <searchPath> <modulePath>"
    
main = do
    args <- getArgs
    case args of
      ["verify",searchPath,modulePath] -> verify [searchPath] modulePath
      ["haskell",searchPath,modulePath] -> haskell [searchPath] modulePath
      _ -> usage