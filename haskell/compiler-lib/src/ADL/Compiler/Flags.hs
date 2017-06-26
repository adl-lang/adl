{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Flags where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Compiler.EIO
import ADL.Compiler.Utils
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags)
import System.Console.GetOpt

import qualified Paths_adl_compiler_lib as P

-- Hold a set of flags for the adl frontend, a given backend
-- and the output writer
data Flags b = Flags {
  f_adl :: AdlFlags,
  f_output :: OutputArgs,
  f_backend :: b
  }

updateAdlFlags :: (AdlFlags -> AdlFlags) -> Flags b -> Flags b
updateAdlFlags fn flags = flags{f_adl=fn (f_adl flags)}

updateOutputArgs :: (OutputArgs -> OutputArgs) -> Flags b -> Flags b
updateOutputArgs fn flags = flags{f_output=fn (f_output flags)}

updateBackendFlags :: (b -> b) -> Flags b -> Flags b
updateBackendFlags fn flags = flags{f_backend=fn (f_backend flags)}

defaultOutputArgs :: OutputArgs
defaultOutputArgs = OutputArgs {
  oa_log = const (return ()),
  oa_noOverwrite = True,
  oa_outputPath = "."
  }

searchDirOption ufn =
  Option "I" ["searchdir"]
    (ReqArg ufn "DIR")
    "Add the specifed directory to the ADL searchpath"

addToSearchPath :: FilePath -> Flags b -> Flags b
addToSearchPath path = updateAdlFlags (\af-> af{af_searchPath=path:af_searchPath af})

mergeFileExtensionOption ufn =
  Option "" ["merge-adlext"]
    (ReqArg ufn "EXT")
    "Add the specifed adl file extension to merged on loading"

addToMergeFileExtensions :: String -> Flags b -> Flags b
addToMergeFileExtensions ext = updateAdlFlags (\af-> af{af_mergeFileExtensions=ext:af_mergeFileExtensions af})

verboseOption ufn =
  Option "" ["verbose"]
    (NoArg ufn)
    "Print extra diagnostic information, especially about files being read/written"

setVerbose :: Flags b -> Flags b
setVerbose = updateOutputArgs (\oa-> oa{oa_log=putStrLn})
           . updateAdlFlags (\af-> af{af_log=putStrLn})

outputDirOption ufn =
  Option "O" ["outputdir"]
    (ReqArg ufn "DIR")
    "Set the directory where generated code is written"

setOutputDir :: FilePath -> Flags b -> Flags b
setOutputDir dir = updateOutputArgs (\oa -> oa{oa_outputPath=dir})

noOverwriteOption ufn =
  Option "" ["no-overwrite"]
    (NoArg ufn)
    "Don't update files that haven't changed"

setNoOverwrite :: Flags b -> Flags b
setNoOverwrite = updateOutputArgs (\oa-> oa{oa_noOverwrite=True})

-- | Combine an initial set of AdlFlags and appropriate backend
-- flags with command line arguments.
buildFlags :: AdlFlags -> b -> [Flags b -> Flags b] -> Flags b
buildFlags af0 b0 opts = ((foldl (.) id opts) (Flags af0 defaultOutputArgs b0))


standardOptions :: [OptDescr (Flags b -> Flags b)]
standardOptions =
  [ searchDirOption addToSearchPath
  , outputDirOption setOutputDir
  , mergeFileExtensionOption addToMergeFileExtensions
  , verboseOption setVerbose
  , noOverwriteOption setNoOverwrite
  ]

parseArguments :: String -> AdlFlags -> b -> [OptDescr (Flags b -> Flags b)] -> [String] -> EIO T.Text (Flags b,[FilePath])
parseArguments header adlflags0 flags0 optdescs args = do
  case getOpt Permute optdescs args of
    (opts,paths,[]) -> do
      let flags = buildFlags adlflags0 flags0 opts
      return (flags,paths)
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optdescs))
