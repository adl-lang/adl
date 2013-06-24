module ADL.Compiler.Utils where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T

writeFileIfRequired :: Bool -> FilePath -> T.Text -> IO ()
writeFileIfRequired False fpath t = do
  putStrLn ("writing " ++ fpath ++ "...")
  T.writeFile fpath t
writeFileIfRequired True fpath t = do
  t0 <- catch (T.readFile fpath) ((\_ -> return T.empty) :: IOError -> IO T.Text)
  if t /= t0
     then do
       putStrLn ("writing " ++ fpath ++ ".")
       T.writeFile fpath t
    else
       putStrLn ("leaving " ++ fpath ++ " unchanged.")
        
          
