import Control.Monad
import System.Directory
import System.FilePath

import Data.List

import qualified Data.ByteString as BS
import qualified Data.Set as Set

dirContents :: FilePath -> IO (Set.Set FilePath)
dirContents root = scan ""
  where
    scan path = do
      ps <- getDirectoryContents (root </> path)
      dirs <- filterM (\p -> doesDirectoryExist (root </> path </> p)) ps
      files <- filterM (\p -> doesFileExist (root </> path </> p)) ps
      let dirs' = filter (\p -> p /= "." && p /= "..") dirs
      ds <- sequence [ fmap (Set.map (dir</>)) (scan (path </> dir)) | dir <- dirs']
      return (Set.union (Set.fromList files) (Set.unions ds))

generate name dir = do
  putStrLn (name ++ "=\\")
  files <- dirContents dir
  forM_ (filter isValidFile (Set.toList files)) $ \ file ->
    putStrLn (dir ++ "/" ++ file ++ "\\")
  putStrLn ""

isValidFile f = not ("dist" `isPrefixOf` f) &&
                not ("~" `isSuffixOf` f)
  
main = do
  generate "UTILS-SRC" "utils"
  generate "COMPILER-LIB-SRC" "compiler-lib"
  generate "COMPILER-BOOTSTRAP-SRC" "compiler-bootstrap"
  generate "RUNTIME-SRC" "runtime"
  generate "COMPILER-SRC" "compiler"
  generate "EXAMPLE-SRC" "examples"
