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
        [ "adl/examples/echo.adl"
        , "adl/examples/kvstore1.adl"
        , "adl/examples/kvstore2.adl"
        ]
          
      cmd = "adlc haskell --no-overwrite -I adl -I ../lib/adl -O " ++ odir ++ " --moduleprefix=ADL " ++ intercalate " " adlFiles
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
       