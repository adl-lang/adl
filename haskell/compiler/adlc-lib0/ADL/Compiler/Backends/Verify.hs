{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Verify(
  verify,
  ) where

import qualified Data.Text as T

import Control.Monad

import ADL.Compiler.EIO
import ADL.Compiler.Processing

verify :: AdlFlags -> [FilePath] -> EIO T.Text ()
verify af modulePaths = catchAllExceptions $ forM_ modulePaths $ \modulePath -> do
  loadAndCheckModule (moduleFinder af) modulePath

