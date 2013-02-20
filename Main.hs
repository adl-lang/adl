module Main where

import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)
import Control.Monad.Trans
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text as T

import EIO
import Format
import AST
import Processing

verify modulePath searchPaths = do
    e <- unEIO actions
    case e of
        (Left perr) -> putStrLn perr
        (Right ()) -> return ()
  where
    actions = do
      (m,mm) <- loadModule1 
      mapM_ checkModuleForDuplicates (Map.elems mm)
      checkModuleForDuplicates m
      ns <- resolveN (sortByDeps (Map.elems mm)) emptyNameScope
      (_,rm) <- resolve1 m ns
      return ()

    loadModule1 :: EIO String (SModule, SModuleMap)
    loadModule1 = mapError show $ loadModule modulePath (moduleFinder searchPaths) Map.empty

    checkModuleForDuplicates :: SModule -> EIO String ()
    checkModuleForDuplicates m = case dups of
        [] -> return ()
        _ -> eioError (moduleErrorMessage m (map format dups))
      where
        dups = checkDuplicates m

    resolveN :: [SModule] -> NameScope -> EIO String NameScope
    resolveN [] ns = return ns
    resolveN (m:ms) ns = do
        (ns',rm) <- resolve1 m ns
        resolveN ms ns'
    
    resolve1 :: SModule -> NameScope -> EIO String (NameScope,RModule)
    resolve1 m ns = do
        liftIO $ putStrLn ("processing " ++ format (m_name m) ++ "...")
        checkUndefined1 m ns
        let rm = resolveModule m ns
            mdecls = Map.mapKeys (\i -> ScopedName (m_name rm) i) (m_decls rm)
            ns' = ns{ns_globals=Map.union (ns_globals ns) mdecls}
        checkTypeCtorApps1 rm
        return (ns', rm)

    checkUndefined1 :: SModule -> NameScope -> EIO String ()
    checkUndefined1 m ns = case undefinedNames m ns of
        [] -> return ()
        udefs -> eioError (moduleErrorMessage m (map format udefs))

    checkTypeCtorApps1 :: RModule -> EIO String ()
    checkTypeCtorApps1 m = case checkTypeCtorApps m of
        [] -> return ()      
        errs -> eioError (moduleErrorMessage m (map format errs))

    moduleErrorMessage m ss = "In module " ++ format (m_name m) ++ ":\n  " ++
                              intercalate "\n  " ss
          
    emptyNameScope = NameScope Map.empty Map.empty Map.empty Set.empty

sortByDeps :: [SModule] -> [SModule]
sortByDeps ms = map fst (sort0 (modulesWithDeps ms) Set.empty)
  where
    sort0 :: [(SModule,Set.Set ModuleName)] -> Set.Set ModuleName -> [(SModule,Set.Set ModuleName)]
    sort0 [] _ = []
    sort0 ms sofar = let (ok,todo) = partition (\(m,md)-> Set.null (md `Set.difference` sofar)) ms
                     in ok ++ sort0 todo (Set.unions (sofar:map snd ok))
                         
    modulesWithDeps ms = map (\m -> (m,getReferencedModules m)) ms

        
    
moduleFinder :: [FilePath] -> ModuleName -> [FilePath]
moduleFinder rootpaths (ModuleName mname) =
    [ joinPath ([path]++names++[name0++".adl"]) | path <- rootpaths ]
  where
    names = map T.unpack (init mname)
    name0 = T.unpack (last mname)
      
usage = do
    putStrLn "Usage: adl verify <searchPath> <modulePath>"
    
main = do
    args <- getArgs
    case args of
      ["verify",searchPath,modulePath] -> verify modulePath [searchPath]
      _ -> usage