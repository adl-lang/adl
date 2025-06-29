{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Verify(
  verify,
  ) where

import qualified Data.Text as T

import Control.Monad

import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.AST

verify :: AdlFlags -> [ModuleName] -> EIO T.Text ()
verify af moduleNames = catchAllExceptions $ forM_ moduleNames $ \moduleName -> do
  lc <- buildLoadContext af
  loadAndCheckRModules lc [moduleName]

