module ADL.Compiler.DataFiles(
  getLibDir,
  javaRuntimeDir,
  systemAdlDir,
  stdlibCustomTypesHs,
  stdlibCustomTypesCpp,
  stdlibCustomTypesJava
  ) where

import System.FilePath
import qualified Paths_adl_compiler as P


getLibDir :: IO FilePath
getLibDir = do
  -- If we are falling back on cabal data files,
  -- cheat and use path relative to a file we know exists
  ref <- P.getDataFileName "lib/adl/sys/types.adl"
  return (takeDirectory (takeDirectory (takeDirectory ref)))

javaRuntimeDir :: FilePath -> FilePath
javaRuntimeDir libdir = libdir </> "java/runtime"

systemAdlDir :: FilePath -> FilePath
systemAdlDir libdir = libdir </> "adl"

stdlibCustomTypesHs :: FilePath -> FilePath
stdlibCustomTypesHs libdir = libdir </> "adl/sys/types/hs-custom-types.json"

stdlibCustomTypesCpp :: FilePath -> FilePath
stdlibCustomTypesCpp libdir = libdir </> "adl/sys/types/cpp-custom-types.json"

stdlibCustomTypesJava :: FilePath -> FilePath
stdlibCustomTypesJava libdir = libdir </> "adl/sys/types/java-custom-types.json"
