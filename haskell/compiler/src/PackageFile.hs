module PackageFile(
  packageFileName,
  packageDepsFromPackageFile,
) where

import System.FilePath(joinPath)
import ADL.Compiler.EIO
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import ADL.Compiler.Processing (LogFn)
import ADL.Core (adlFromJsonFile')
import ADL.Adlc.Package (AdlPackage(..), AdlPackageRef(..))
import qualified Data.Text as T

packageFileName = "adl-package.json"

packageDepsFromPackageFile :: LogFn -> FilePath -> EIOT [FilePath]
packageDepsFromPackageFile log root = do
  let packageFilePath = joinPath [root, packageFileName]
  exists <- liftIO $ doesFileExist packageFilePath
  if exists
    then do
      liftIO  $ log ("Reading " <> packageFilePath <> "...")
      package <- liftIO $ adlFromJsonFile' packageFilePath
      let deps = adlPackage_dependencies package
      liftIO $ log ("package " <> T.unpack (adlPackage_name package) <> " has " <> show (length deps) <> " dependencies")
      pure (fmap (\p -> joinPath [root,p]) (mconcat (fmap getPackageDirs deps)))
    else
      pure []

getPackageDirs :: AdlPackageRef -> [FilePath]
getPackageDirs (AdlPackageRef_localdir dir) = [T.unpack dir]
