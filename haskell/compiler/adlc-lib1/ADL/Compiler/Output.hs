{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Output where

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans

import ADL.Compiler.Utils(FileWriter, LogFn, OutputArgs(..),WriterWithManifest(..), writerWithManifest)
import ADL.Adlc.Config.Codegen(OutputParams(..))
import ADL.Core.Nullable(Nullable(..))
import ADL.Compiler.EIO

withWriter:: OutputParams -> LogFn -> (FileWriter -> EIO e a) -> EIO e a
withWriter op logfn ma = do
  wwm <- liftIO $ writerWithManifest (argsFromOutputParams op logfn)
  a <- ma (wwm_writeFile wwm)
  liftIO $ wwm_writeManifest wwm
  return a

argsFromOutputParams :: OutputParams -> LogFn -> OutputArgs
argsFromOutputParams op logfn = OutputArgs {
  oa_log = logfn,
  oa_noOverwrite = outputParams_noOverwrite op,
  oa_outputPath = T.unpack (outputParams_outputDir op),
  oa_manifestFile = fmap T.unpack (unNullable (outputParams_manifest op))
}

-- | Read the contents of the specified file, parsing it
-- as a json serialised ADL value.
adlFromJsonFile :: AdlValue a => FilePath -> IO (Either T.Text a)
adlFromJsonFile file = (decodeAdlParseResult from . adlFromJsonByteString) <$> (LBS.readFile file)
  where
    from = " from " <> T.pack file

-- | Parse a ByteString containing json to an ADL value
adlFromJsonByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
adlFromJsonByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
  (Left e) -> ParseFailure ("Invalid json:" <> T.pack (Y.prettyPrintParseException e)) []
  (Right jv) -> runJsonParser jsonParser [] jv
