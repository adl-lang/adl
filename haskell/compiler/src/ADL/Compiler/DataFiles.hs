module ADL.Compiler.DataFiles(
  getJavaRuntimeDir,
  getSystemAdlDir,
  getStdlibCustomTypesHs,
  getStdlibCustomTypesCpp,
  getStdlibCustomTypesJava
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

getJavaRuntimeDir :: IO FilePath
getJavaRuntimeDir = (</>"runtime/java") <$> getDataFileDir

getSystemAdlDir :: IO FilePath
getSystemAdlDir = (</>"adl") <$> getDataFileDir

getStdlibCustomTypesHs :: IO FilePath
getStdlibCustomTypesHs = getDataFileName "config/hs-custom-types.json"

getStdlibCustomTypesCpp :: IO FilePath
getStdlibCustomTypesCpp = getDataFileName "config/cpp-custom-types.json"

getStdlibCustomTypesJava :: IO FilePath
getStdlibCustomTypesJava = getDataFileName "config/java-custom-types.json"
