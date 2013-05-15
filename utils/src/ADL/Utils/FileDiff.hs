module ADL.Utils.FileDiff where

import Control.Monad
import System.Directory
import System.FilePath

import qualified Data.ByteString as BS
import qualified Data.Set as Set

data FileDiff = Match
                   | Diff
                   | Missing1
                   | Missing2
    deriving(Eq) 
             
compareFiles :: FilePath -> FilePath -> IO FileDiff
compareFiles fp1 fp2 = do
  e1 <- doesFileExist fp1
  e2 <- doesFileExist fp2
  case (e1,e2) of
    (False,False) -> return Diff
    (False,True) -> return Missing1
    (True,False) -> return Missing2
    (True,True) -> do
      bs1 <- BS.readFile fp1
      bs2 <- BS.readFile fp2
      if bs1 == bs2
        then return Match
        else return Diff

dirContents :: FilePath -> IO (Set.Set FilePath)
dirContents root = scan ""
  where
    scan path = do
      ps <- getDirectoryContents (root </> path)
      dirs <- filterM (\p -> doesDirectoryExist (root </> path </> p)) ps
      files <- filterM (\p -> doesFileExist (root </> path </> p)) ps
      let dirs' = filter (\p -> p /= "." && p /= "..") dirs
      ds <- sequence [scan (path </> dir) | dir <- dirs']
      return (Set.map (path</>)
                      (Set.union (Set.fromList files) (Set.unions ds)))

diffTree :: FilePath -> FilePath -> IO [ (FilePath,FileDiff) ]
diffTree root1 root2 = do
  files1 <- dirContents root1
  files2 <- dirContents root2
  let files = Set.union files1 files2
  diffs <- mapM cmpf (Set.toList files)
  return (filter ((/=Match).snd) diffs)
  where
    cmpf p = do
      r <- compareFiles (root1 </> p) (root2 </> p)
      return (p,r)
  
      