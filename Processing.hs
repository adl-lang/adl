{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Processing where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Exception
import Data.List(intercalate)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Text.Parsec as P
import qualified ParserP as P
import AST

type SModule = Module ScopedName
type SModuleMap = Map.Map ModuleName SModule

newtype EIO e a = EIO { unEIO :: IO (Either e a) }

instance Monad (EIO e) where
    return a = EIO (return (Right a))
    (EIO mea) >>= fmb = EIO $ do
        ea <- mea
        case ea of
            (Left e) -> return (Left e)
            (Right a) -> unEIO (fmb a)

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
    
loadModule :: FilePath -> (ModuleName -> [FilePath]) -> SModuleMap -> EIO P.ParseError (SModule,SModuleMap)
loadModule fpath findm mm = do
    m0 <- eioFromEither $ P.fromFile P.moduleFile fpath
    mm' <- addDeps m0 mm
    return (m0,mm')

  where
    addDeps :: SModule -> SModuleMap -> EIO P.ParseError SModuleMap
    addDeps m mm = do
       liftIO $ print (getReferencedModules m)
       foldM addDep mm (Set.toList (getReferencedModules m))

    addDep :: SModuleMap -> ModuleName -> EIO P.ParseError SModuleMap
    addDep mm mname = case Map.member mname mm of
        True -> return mm
        False -> do
          m <- findModule mname (findm mname)
          let mm' = Map.insert mname m mm
          addDeps m mm'

    findModule :: ModuleName -> [FilePath] -> EIO P.ParseError SModule
    findModule mname [] = liftIO $ ioError $ userError $ "Unable to find module '" ++ T.unpack (moduleName mname) ++ "'"
    findModule mname (fpath:fpaths) = do
        em <-  liftIO $ try (P.fromFile P.moduleFile fpath)
        case em of
            (Left (ioe::IOError)) -> findModule mname fpaths
            (Right em) -> eioFromEither (return em)

type ErrorLog = [T.Text]
      
checkDuplicates :: Module t -> ErrorLog
checkDuplicates m = declErrors ++ structErrors ++ unionErrors
  where
    declErrors = map dupMessage1 $ findDuplicates [ d_name d | d <- Map.elems (m_decls m) ]
    structErrors = concat [ structErrors1 n s | Decl{d_name=n,d_type=Decl_Struct s} <- Map.elems (m_decls m) ]
    unionErrors = concat [ unionErrors1 n u | Decl{d_name=n,d_type=Decl_Union u} <- Map.elems (m_decls m) ]

    structErrors1 n s = (map (dupMessage2 "struct" "field" n) . findDuplicates) [ f_name f | f <- s_fields s ] ++
                        (map (dupMessage2 "struct" "type param" n) . findDuplicates) [t | t <- s_typeParams s ]

    unionErrors1 n u = (map (dupMessage2 "union" "field" n) . findDuplicates) [ f_name f | f <- u_fields u ] ++
                       (map (dupMessage2 "union" "type param" n) . findDuplicates) [t | t <- u_typeParams u ]

    dupMessage2 s1 s2 n i = T.concat [ mname (m_name m), ", in ", s1, " ", n, " duplicate ", s2, " ", i ]
    dupMessage1 i = T.concat [ mname (m_name m), ", duplicate definition of ", i ]
      
    mname :: ModuleName -> T.Text
    mname m = "In module " `T.append` moduleName m

    findDuplicates :: (Ord a) => [a] -> [a]
    findDuplicates as = [ a | (a,n) <- Map.toList (foldr (\a -> Map.insertWith' (+) a 1) Map.empty as),
                          n > 1 ]

moduleName :: ModuleName -> T.Text
moduleName m = T.intercalate "." m

newtype NameRefDecl = NameRefDecl (Decl ScopedName)
newtype ResolvedDecl = ResolvedDecl (Decl ResolvedDecl)

resolveTypes :: Map.Map ScopedName NameRefDecl
             -> Map.Map ScopedName ResolvedDecl
resolveTypes map = rmap
  where
    rmap = Map.map resolve1 map

    resolve1 :: NameRefDecl -> ResolvedDecl
    resolve1 (NameRefDecl d) = ResolvedDecl d{d_type=resolve2 (d_type d)}

    resolve2 :: DeclType ScopedName -> DeclType ResolvedDecl
    resolve2 = undefined

