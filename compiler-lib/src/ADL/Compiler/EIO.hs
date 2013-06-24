module ADL.Compiler.EIO where

import Data.Either
import Control.Exception
import Control.Monad.Trans

newtype EIO e a = EIO { unEIO :: IO (Either e a) }

instance Monad (EIO e) where
    return a = EIO (return (Right a))
    (EIO mea) >>= fmb = EIO $ do
        ea <- mea
        case ea of
            (Left e) -> return (Left e)
            (Right a) -> unEIO (fmb a)

instance Functor (EIO e) where
   fmap f (EIO mea) =  EIO $ do
        ea <- mea
        case ea of
            (Left e) -> return (Left e)
            (Right a) -> return (Right (f a))

instance MonadIO (EIO e) where
    liftIO a = EIO (fmap Right a)

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