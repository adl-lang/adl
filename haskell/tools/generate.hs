#!/usr/bin/env stack
{- stack --install-ghc runghc -}
{-# LANGUAGE OverloadedStrings #-}

-- Run adlc-bootstrap compiler to regenerate the haskell code for ADL
-- specified types.
--
-- It would be nice to have this within the stack build process, but
-- stack/cabal still doesn't support dependencies on build tools
-- properly.

import Data.Monoid
import System.FilePath((</>))
import System.Process(system)

adlstdlibdir = "compiler/lib/adl"

compile compiler args adldir outdir path0 = do
  let path = adldir </> path0
  putStrLn
    (  "stack exec " <> compiler <> " -- haskell "
    <> "--package=ADL "
    <> "--no-overwrite "
    <> "-O " <> outdir <> " "
    <> "-I " <> adldir <> " "
    <> "-I " <> adlstdlibdir <> " "
    <> args
    <> path
    )

adlcb = compile "adlc-bootstrap" ""
adlc = compile "adlc" "--verbose "

main = do
  putStrLn "# Run the following:"

  -- compiler annotation types
  adlcb "compiler/lib/adl/adlc/config" "compiler/src" "haskell.adl"
  adlcb "compiler/lib/adl/adlc/config" "compiler/src" "cpp.adl"
  adlcb "compiler/lib/adl/adlc/config" "compiler/src" "java.adl"
  adlcb "compiler/lib/adl/adlc/config" "compiler/src" "typescript.adl"
  adlcb "compiler/lib/adl/adlc/config" "compiler/src" "rust.adl"

  -- runtime
  adlcb adlstdlibdir "runtime/src" "sys/types.adl"
  adlcb adlstdlibdir "runtime/src" "sys/adlast.adl"

  -- typescript runtime
  putStrLn
    (  "stack exec adlc -- typescript "
    <> "--no-overwrite "
    <> "--exclude-ast "
    <> "--verbose "
    <> "-O ../typescript/runtime "
    <> "-I compiler/lib/adl "
    <> "compiler/lib/adl/sys/types.adl compiler/lib/adl/sys/adlast.adl compiler/lib/adl/sys/dynamic.adl"
    )
