module ADL.Compiler.EIO where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception
import Control.Monad.Trans
import System.Exit(exitWith,ExitCode(..))
import System.IO(stderr)

newtype EIO e a = EIO { unEIO :: IO (Either e a) }

instance Functor (EIO e) where
   fmap f (EIO mea) =  EIO $ do
        ea <- mea
        case ea of
            (Left e) -> return (Left e)
            (Right a) -> return (Right (f a))

instance Monad (EIO e) where
    (EIO mea) >>= fmb = EIO $ do
        ea <- mea
        case ea of
            (Left e) -> return (Left e)
            (Right a) -> unEIO (fmb a)

instance Applicative (EIO e) where
    pure a = EIO (return (Right a))
    af <*> aa = do {f <- af; a <- aa; return (f a)}

instance MonadIO (EIO e) where
    liftIO a = EIO (fmap Right a)

type EIOT a = EIO T.Text a

eioError :: e -> EIO e a
eioError e = EIO (return (Left e))

eioFromEither :: IO (Either e a) -> EIO e a
eioFromEither mea = do
    ea <- liftIO mea
    case ea of
        (Left e) -> eioError e
        (Right a) -> return a

mapError :: (a->b) -> EIO a c -> EIO b c
mapError f (EIO e) = EIO $ fmap (either (Left . f) (Right . id)) e

eioHandle :: Exception x => (x -> EIO e a) -> EIO e a -> EIO e a
eioHandle h a = EIO (handle (\x -> unEIO (h x)) (unEIO a))

catchAllExceptions :: EIO T.Text a -> EIO T.Text a
catchAllExceptions a = eioHandle handler $ a
 where
   handler :: SomeException -> EIO T.Text a
   handler e = eioError (T.pack (show e))

exitOnError :: EIO T.Text a -> IO a
exitOnError eio = do
  eresult <- unEIO eio
  case eresult of
   Left err -> do
     T.hPutStrLn stderr err
     exitWith (ExitFailure 1)
   Right v -> return v
