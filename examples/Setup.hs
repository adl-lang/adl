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
--        , "adl/examples/kvstore3.adl"
        , "adl/examples/pubsub.adl"
        , "adl/examples/pubsub1.adl"
        , "adl/examples/datetime.adl"
        , "adl/examples/serialisation.adl"
        ]
          
      cmd = "adlc haskell --custom-types config/hs-custom-types.json --no-overwrite -I adl -I ../runtime/adl -O " ++ odir ++ " --moduleprefix=ADL " ++ intercalate " " adlFiles
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
       