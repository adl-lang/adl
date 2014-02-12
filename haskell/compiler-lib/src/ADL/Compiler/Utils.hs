module ADL.Compiler.Utils(
  OutputArgs(..),
  writeOutputFile
  )where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory(createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

data OutputArgs = OutputArgs {
  oa_log :: String -> IO (),
  oa_noOverwrite :: Bool,
  oa_outputPath :: FilePath
  }                   
  
writeOutputFile :: OutputArgs -> FilePath -> LBS.ByteString -> IO ()
writeOutputFile (oa@OutputArgs{oa_noOverwrite=False}) fpath0 t = do
  let fpath = oa_outputPath oa </> fpath0
  oa_log oa ("writing " ++ fpath ++ "...")
  createDirectoryIfMissing True (takeDirectory fpath)
  LBS.writeFile fpath t
writeOutputFile (oa@OutputArgs{oa_noOverwrite=True}) fpath0 t = do
  let fpath = oa_outputPath oa </> fpath0
  t0 <- catch (BS.readFile fpath) ((\_ -> return BS.empty) :: IOError -> IO BS.ByteString)
  if t /= LBS.fromStrict t0
     then do
       oa_log oa ("writing " ++ fpath ++ ".")
       createDirectoryIfMissing True (takeDirectory fpath)
       LBS.writeFile fpath t
    else
       oa_log oa ("leaving " ++ fpath ++ " unchanged.")
        
          
