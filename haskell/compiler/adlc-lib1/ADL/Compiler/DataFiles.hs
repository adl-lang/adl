module ADL.Compiler.DataFiles(
  getLibDir,
  javaRuntimeDir,
  typescriptRuntimeDir,
  haskellRuntimeDir,
  rustRuntimeDir,
  systemAdlDir,
  stdlibCustomTypesHs,
  stdlibCustomTypesCpp
  ) where

import qualified Paths_adl_compiler as P

import System.Directory(doesFileExist,doesDirectoryExist)
import System.Environment.Executable(getExecutablePath)
import System.Environment(lookupEnv)
import System.FilePath

getLibDir :: IO FilePath
getLibDir = do
  -- First we check for an ADL_ROOT environment variable, which
  -- must point to a directory containing the lib directory.
  ms <- lookupEnv "ADL_ROOT"
  case ms of
    (Just adlRoot) -> return (adlRoot </> "lib")
    Nothing -> do
      -- Next try a path relative to the binary
      exePath <- getExecutablePath
      let adlRoot = takeDirectory (takeDirectory exePath)
      exists <- doesDirectoryExist (adlRoot </> "lib/adl")
      if exists
         then return (adlRoot </> "lib")
         else do
          -- Fall back on cabal installed files in the source tree
          ref <- P.getDataFileName "lib/adl/sys/types.adl"
          exists <- doesFileExist ref
          if exists
             then return (takeDirectory (takeDirectory (takeDirectory ref)))
             else error "Unable to find lib directory"

javaRuntimeDir :: FilePath -> FilePath
javaRuntimeDir libdir = libdir </> "java/runtime"

haskellRuntimeDir :: FilePath -> FilePath
haskellRuntimeDir libdir = libdir </> "haskell/runtime"

systemAdlDir :: FilePath -> FilePath
systemAdlDir libdir = libdir </> "adl"

stdlibCustomTypesHs :: FilePath -> FilePath
stdlibCustomTypesHs libdir = libdir </> "adl/sys/types/hs-custom-types.json"

stdlibCustomTypesCpp :: FilePath -> FilePath
stdlibCustomTypesCpp libdir = libdir </> "adl/sys/types/cpp-custom-types.json"

typescriptRuntimeDir :: FilePath -> FilePath
typescriptRuntimeDir libdir = libdir </> "typescript/runtime/embedded"

rustRuntimeDir :: FilePath -> FilePath
rustRuntimeDir libdir = libdir </> "rust/runtime"
