module Main where

import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate)
import qualified Data.Map as Map

import qualified Data.Text as T

import AST
import Processing
import EIO

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
      return ()

    loadModule1 :: EIO String (SModule, SModuleMap)
    loadModule1 = mapError show $ loadModule modulePath (moduleFinder searchPaths) Map.empty

    checkModuleForDuplicates :: Module ScopedName -> EIO String ()
    checkModuleForDuplicates m = case dups of
        [] -> return ()
        _ -> eioError (dupMessage dups)
      where
        dups = checkDuplicates m
        dupMessage dups = "In module " ++ T.unpack (moduleName (m_name m)) ++ ":\n  " ++
                            intercalate "\n  " (map dupMessage0 dups)
        dupMessage0 (D_Decl n) = ds ++ T.unpack n
        dupMessage0 (D_StructField s f) = ds ++ "field " ++ T.unpack f ++ " in struct " ++ T.unpack s
        dupMessage0 (D_StructParam s p) = ds ++ "type parameter " ++ T.unpack p ++ " in struct " ++ T.unpack s
        dupMessage0 (D_UnionField u f) = ds ++ "field " ++ T.unpack f ++ " in union " ++ T.unpack u
        dupMessage0 (D_UnionParam u p) = ds ++ "type parameter " ++ T.unpack p ++ " in union " ++ T.unpack u
        ds = "duplicate definition of "
  

moduleFinder :: [FilePath] -> ModuleName -> [FilePath]
moduleFinder rootpaths mname = [ joinPath ([path]++names++[name0++".adl"])
                               | path <- rootpaths ]
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