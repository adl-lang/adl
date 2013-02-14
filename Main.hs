module Main where

import System.Environment (getArgs)
import System.FilePath(joinPath)
import qualified Data.Map as Map

import qualified Data.Text as T

import AST
import Processing
import EIO

verify modulePath searchPaths = do
  em <-unEIO (loadModule modulePath (moduleFinder searchPaths) Map.empty)
  case em of
    (Left perr) -> print perr
    (Right (m,mm)) -> doverify m mm

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