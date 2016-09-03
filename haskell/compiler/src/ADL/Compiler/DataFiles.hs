module ADL.Compiler.DataFiles(
  getDataFileName,
  getDataFileDir
  ) where

import System.FilePath
import qualified Paths_adl_compiler as P


getDataFileName :: FilePath -> IO FilePath
getDataFileName relFilePath = do
  base <- getDataFileDir
  return (base </> relFilePath)

getDataFileDir :: IO FilePath
getDataFileDir = do
  -- If we are falling back on cabal data files,
  -- cheat and use path relative to a file we know exists
  ref <- P.getDataFileName "config/hs-custom-types.json"
  return (takeDirectory (takeDirectory ref))

