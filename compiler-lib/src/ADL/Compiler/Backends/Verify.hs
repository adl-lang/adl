{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Verify(
  verify,
  VerifyFlags(..),
  ) where

import qualified Data.Text as T

import Control.Monad

import ADL.Compiler.EIO
import ADL.Compiler.Processing

data VerifyFlags = VerifyFlags {
  vf_searchPath :: [FilePath]
}

verify :: VerifyFlags -> [FilePath] -> EIO T.Text ()
verify vf modulePaths = catchAllExceptions $ forM_ modulePaths $ \modulePath -> do
  loadAndCheckModule (moduleFinder (vf_searchPath vf)) modulePath

