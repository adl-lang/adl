{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)
import Control.Monad
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

data VerifyFlags = VerifyFlags {
  vf_searchPath :: [FilePath]
}

searchDirOption ufn =
  Option "I" ["searchdir"]
    (ReqArg ufn "DIR")
    "Add the specifed directory to the ADL searchpath"

outputDirOption ufn =
  Option "O" ["outputdir"]
    (ReqArg ufn "DIR")
    "Set the directory where generated code is written"

verify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> run (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    run vf args = forM_ args $ \modulePath -> do
      loadAndCheckModule (moduleFinder (vf_searchPath vf)) modulePath

    header = "Usage: adl verify [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (VerifyFlags [])

    optDescs =
      [ searchDirOption (\s vf-> vf{vf_searchPath=s:vf_searchPath vf})
      ]

data HaskellFlags = HaskellFlags {
  hf_searchPath :: [FilePath],
  hf_modulePrefix :: String,
  hf_outputPath :: FilePath
}

haskell args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> run (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    run hf args = forM_ args $ \modulePath -> do
      rm <- loadAndCheckModule (moduleFinder (hf_searchPath hf)) modulePath
      H.writeModuleFile (H.moduleMapper (hf_modulePrefix hf))
                        (H.fileMapper (hf_outputPath hf)) rm
 
    header = "Usage: adl haskell [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (HaskellFlags [] "ADL.Generated" ".")

    optDescs =
      [ searchDirOption (\s hf-> hf{hf_searchPath=s:hf_searchPath hf})
      , Option "" ["moduleprefix"]
        (ReqArg (\s hf-> hf{hf_modulePrefix=s}) "PREFIX")
        "Set module name prefix for generated code "
      , outputDirOption (\s hf-> hf{hf_outputPath=s})
      ]

usage = T.intercalate "\n"
  [ "Usage: adl verify [OPTION..] <modulePath>..."
  , "       adl haskell [OPTION..] <modulePath>..."
  ]    
    
main = do
  args <- getArgs
  runEIO $ case args of
    ("verify":args) -> verify args
    ("haskell":args) -> haskell args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.putStrLn perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess


      