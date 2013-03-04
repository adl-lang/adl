import Distribution.Simple(defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils(rewriteFile)
import Distribution.Simple.BuildPaths(autogenModulesDir)
import System.FilePath((</>))
import Data.Version(showVersion)
import System.Process(system)
import System.Exit
import Data.List(intercalate)

generateHaskellFromADL pkg lbi = do
  let odir = autogenModulesDir lbi
      adlFiles =
        [ "adl/sys/types.adl"
        , "adl/sys/rpc.adl"
        ]
          
      cmd = "adlc haskell -I adl -O " ++ odir ++ " --moduleprefix=ADL " ++ intercalate " " adlFiles
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