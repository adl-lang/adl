{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Verify(
  verify,
  ) where

import qualified Data.Text as T

import Control.Monad

import ADL.Compiler.EIO
import ADL.Compiler.Processing

verify :: AdlFlags -> [FilePath] -> EIO T.Text ()
verify af modulePaths = catchAllExceptions $ do
  loadAndCheckModules (moduleFinder af) modulePaths
  return ()

