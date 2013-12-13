import Distribution.Simple(defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils(rewriteFile)
import Distribution.Simple.BuildPaths(autogenModulesDir)
import System.FilePath(combine)
import Data.Version(showVersion)
import System.Process(system)
import System.Exit
import Data.List(intercalate)

generateHaskellFromADL pkg lbi = do
  let adlstdlibdir = " ../../adl/stdlib"
      odir = autogenModulesDir lbi
      adlFiles = map (combine adlstdlibdir)
        [ "sys/types.adl"
        , "sys/rpc.adl"
        , "sys/sinkimpl.adl"
        , "sys/adlast.adl"
        ]
          
      cmd = "adlc-bootstrap haskell --no-overwrite -I" ++ adlstdlibdir ++ " -O " ++ odir ++ " --moduleprefix=ADL " ++ intercalate " " adlFiles
  putStrLn cmd
  e <- system cmd
  case e of
    ExitSuccess -> return ()
    ExitFailure _ -> (exitWith e)

myBuildHook pkg lbi hooks flags = do
  generateHaskellFromADL pkg lbi
  buildHook simpleUserHooks pkg lbi hooks flags

main = defaultMainWithHooks simpleUserHooks {
  buildHook=myBuildHook
  }