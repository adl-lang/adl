{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Batch(
  generate
) where

import ADL.Compiler.EIO
import ADL.Adlc.Codegen.Batch

generate :: FilePath -> EIOT ()
generate batchFile =
  return ()
