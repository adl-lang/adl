module ADL.Compiler.Utils(
  OutputArgs(..),
  writeOutputFile
  )where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory(createDirectoryIfMissing)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data OutputArgs = OutputArgs {
  oa_log :: String -> IO (),
  oa_noOverwrite :: Bool,
  oa_outputPath :: FilePath
  }                   
  
writeOutputFile :: OutputArgs -> FilePath -> T.Text -> IO ()
writeOutputFile (oa@OutputArgs{oa_noOverwrite=False}) fpath0 t = do
  let fpath = oa_outputPath oa </> fpath0
  oa_log oa ("writing " ++ fpath ++ "...")
  createDirectoryIfMissing True (takeDirectory fpath)
  T.writeFile fpath t
writeOutputFile (oa@OutputArgs{oa_noOverwrite=True}) fpath0 t = do
  let fpath = oa_outputPath oa </> fpath0
  t0 <- catch (T.readFile fpath) ((\_ -> return T.empty) :: IOError -> IO T.Text)
  if t /= t0
     then do
       oa_log oa ("writing " ++ fpath ++ ".")
       createDirectoryIfMissing True (takeDirectory fpath)
       T.writeFile fpath t
    else
       oa_log oa ("leaving " ++ fpath ++ " unchanged.")
        
          
