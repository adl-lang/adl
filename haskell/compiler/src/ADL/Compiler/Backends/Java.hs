{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java(
  generate,
  JavaFlags(..),
  CodeGenProfile(..),
  defaultCodeGenProfile,
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Parsec as P

import qualified ADL.Adlc.Config.Java as JC
import qualified ADL.Compiler.ParserP as P

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as JSON
import Data.Char(toUpper)
import Data.Maybe(fromMaybe,isJust)
import Data.Foldable(for_,fold)
import Data.List(intersperse,replicate,sort)
import Data.Monoid
import Data.String(IsString(..))
import Data.Traversable(for)

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Utils.Literals2
import ADL.Compiler.Backends.Utils.IndentedCode
import ADL.Compiler.Backends.Java.Internal
import ADL.Compiler.Backends.Java.Parcelable
import ADL.Core.Value
import ADL.Utils.Format

generate :: JavaFlags -> [FilePath] -> EIOT ()
generate jf modulePaths = catchAllExceptions  $ do
  customTypes <- loadCustomTypes (jf_customTypeFiles jf)
  for_ modulePaths $ \modulePath -> do
    m <- loadAndCheckModule (moduleFinder (jf_searchPath jf)) modulePath
    generateModule (packageGenerator (jf_package jf))
                   (fileGenerator (jf_package jf))
                   (const (jf_codeGenProfile jf))
                   customTypes
                   (jf_fileWriter jf)
                   m


generateModule :: (ModuleName -> JavaPackage) ->
                  (ScopedName -> FilePath) ->
                  (ScopedName -> CodeGenProfile) ->
                  CustomTypeMap ->
                  (FilePath -> LBS.ByteString -> IO ()) ->
                  Module ResolvedType ->
                  EIO T.Text ()
generateModule javaPackageFn mFile mCodeGetProfile customTypes fileWriter m0 = do
  let moduleName = m_name m
      m = ( associateCustomTypes moduleName customTypes
          . removeModuleTypedefs
          . expandModuleTypedefs
          ) m0
      decls = Map.elems (m_decls m)
  for_ decls $ \decl -> do
    let codeProfile = mCodeGetProfile (ScopedName moduleName (d_name decl))
        maxLineLength = cgp_maxLineLength codeProfile
        file = mFile (ScopedName moduleName (unreserveWord (d_name decl)))
    case d_type decl of
      (Decl_Struct s) -> writeClassFile file maxLineLength (generateStruct codeProfile moduleName javaPackageFn decl s)
      (Decl_Union u)  -> writeClassFile file maxLineLength (generateUnion codeProfile moduleName javaPackageFn decl u)
      (Decl_Newtype n) -> writeClassFile file maxLineLength (generateNewtype codeProfile moduleName javaPackageFn decl n)
      (Decl_Typedef _) -> eioError "BUG: typedefs should have been eliminated"
  where
    writeClassFile :: FilePath -> Int -> ClassFile -> EIO a ()
    writeClassFile path maxLineLength cfile = do
      let lines = codeText maxLineLength (classFileCode cfile)
      liftIO $ fileWriter path (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" lines <> "\n")))
      

generateStruct :: CodeGenProfile -> ModuleName -> (ModuleName -> JavaPackage) -> Decl CResolvedType -> Struct CResolvedType -> ClassFile
generateStruct codeProfile moduleName javaPackageFn decl struct =  execState gen state0
  where
    className = unreserveWord (d_name decl)
    state0 = classFile codeProfile moduleName javaPackageFn classDecl 
    isEmpty = null (s_fields struct)
    classDecl = "public class " <> className <> typeArgs
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    gen = do
      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (s_fields struct)

      preventImport className
      for_ fieldDetails (\fd -> preventImport (fd_fieldName fd))
      
      objectsClass <- addImport "java.util" "Objects"

      -- Fields
      for_ fieldDetails $ \fd -> do
        let modifiers =
             (if cgp_publicMembers codeProfile then ["public"] else ["private"])
             <>
             (if cgp_mutable codeProfile then [] else ["final"])
        addField (ctemplate "$1 $2 $3;" [T.intercalate " " modifiers,fd_typeExprStr fd,fd_fieldName fd])

      -- Constructors
      let ctorArgs =  T.intercalate ", " [fd_typeExprStr fd <> " " <> fd_fieldName fd | fd <- fieldDetails]
          isGeneric = length (s_typeParams struct) > 0
          
          ctor1 =
            cblock (template "public $1($2)" [className,ctorArgs]) (
              clineN [
                if needsNullCheck fd
                  then template "this.$1 = $2.requireNonNull($1);" [fd_fieldName fd,objectsClass]
                  else template "this.$1 = $1;" [fd_fieldName fd]
                | fd <- fieldDetails]
            )

          ctor2 =
            cblock (template "public $1()" [className]) (
              clineN [template "this.$1 = $2;" [fd_fieldName fd,fd_defValue fd] | fd <- fieldDetails]
            )

          ctor3 =
            cblock (template "public $1($2 other)" [className, className <> typeArgs]) (
              mconcat [ let n = fd_fieldName fd in ctemplate "this.$1 = $2;" [n,fd_copy fd ("other." <>n)]
                      | fd <- fieldDetails ]
            )

      addMethod (cline "/* Constructors */")

      addMethod ctor1
      when (not isGeneric && not isEmpty) (addMethod ctor2)
      when (not isGeneric) (addMethod ctor3)

      -- Getters/Setters
      when (not isEmpty) (addMethod (cline "/* Accessors and mutators */"))
      
      when (not (cgp_publicMembers codeProfile)) $ do
        for_ fieldDetails $ \fd -> do
          let fieldName = fd_fieldName fd
              capsFieldName = javaCapsFieldName (fd_field fd)
              typeExprStr = fd_typeExprStr fd
              getter =
                cblock (template "public $1 get$2()" [typeExprStr,capsFieldName]) (
                  ctemplate "return $1;" [fieldName]
                )
              setter =
                cblock (template "public void set$1($2 new$1)" [capsFieldName,typeExprStr]) (
                  if needsNullCheck fd
                     then ctemplate "$1 = $2.requireNonNull(new$3);" [fieldName,objectsClass,capsFieldName]
                     else ctemplate "$1 = new$2;" [fieldName,capsFieldName]
                )
          addMethod getter
          when (cgp_mutable codeProfile) (addMethod setter)

      -- equals and hashcode
      addMethod (cline "/* Object level helpers */")

      let equals = coverride "public boolean equals(Object other0)" (
            cblock (template "if (!(other0 instanceof $1))"  [className]) (
              cline "return false;"
              )
            <>
            ctemplate "$1 other = ($1) other0;" [className]
            <>
            cline "return"
            <>
            let terminators = replicate (length fieldDetails-1) " &&" <> [";"]
                tests = [ctemplate (if unboxedField fd then "$1 == other.$1$2" else "$1.equals(other.$1)$2")
                                   [fd_fieldName fd,term]
                        | (fd,term) <- zip fieldDetails terminators]
            in  indent (mconcat tests)
            )
          equalsEmpty = coverride "public boolean equals(Object other)" (
            ctemplate "return other instanceof $1;" [className]
            )
      addMethod (if isEmpty then equalsEmpty else equals)

      addMethod $ coverride "public int hashCode()" (
        cline "int result = 1;"
        <>
        let hashfn fd from = case (f_type (fd_field fd)) of
              (TypeExpr (RT_Primitive pt) []) -> pd_hashfn (genPrimitiveDetails pt) from
              _ -> template "$1.hashCode()" [from]
        in mconcat [ctemplate "result = result * 37 + $1;" [hashfn fd (fd_fieldName fd)] | fd <- fieldDetails]
        <>
        cline "return result;"
        )

      factoryInterface <- addImport (cgp_runtimePackage codeProfile) "Factory"

      -- factory
      let factory =
            cblock1 (template "public static final $2<$1> FACTORY = new $2<$1>()" [className,factoryInterface]) (
              cblock (template "public $1 create()" [className]) (
                 ctemplate "return new $1();" [className]
              )
              <>
              cblock (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
            )

      let factoryg =
            cblock (template "public static $2 $3<$1$2> factory($4)" [className,typeArgs,factoryInterface,factoryArgs]) (
              cblock1 (template "return new $1<$2$3>()" [factoryInterface,className,typeArgs]) (
                mconcat [ctemplate "final $1<$2> $3 = $4;" [factoryInterface,fd_boxedTypeExprStr fd,fd_fieldName fd,fd_factoryExprStr fd] | fd <- fieldDetails, not (immutableType (f_type (fd_field fd)))]
                <>
                cline ""
                <>
                cblock (template "public $1$2 create()" [className,typeArgs]) (
                   ctemplate "return new $1$2(" [className,typeArgs]
                   <>
                   indent (clineN (addTerminators "," "," ""  ctor1Args) <> cline ");")
                   )
                <>
                cline ""
                <>
                cblock (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
                   ctemplate "return new $1$2(" [className,typeArgs]
                   <>
                   indent (clineN (addTerminators "," "," ""  ctor2Args) <> cline ");")
                   )
                )
              )

          factoryArgs = commaSep [template "$1<$2> $3" [factoryInterface,arg,factoryTypeArg arg] | arg <- s_typeParams struct]
          ctor1Args = [if immutableType (f_type (fd_field fd))
                       then fd_defValue fd
                       else template "$1.create()" [fd_fieldName fd]
                      | fd <-fieldDetails]
          ctor2Args = [if immutableType (f_type (fd_field fd))
                       then template "other.$1" [fieldAccessExpr codeProfile fd]
                       else template "$1.create(other.$2)" [fd_fieldName fd,fieldAccessExpr codeProfile fd]
                      | fd <- fieldDetails]

      addMethod (cline "/* Factory for construction of generic values */")

      addMethod (if isGeneric then factoryg else factory)

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        generateStructParcelable codeProfile decl struct fieldDetails

generateNewtype :: CodeGenProfile -> ModuleName -> (ModuleName -> JavaPackage) -> Decl CResolvedType -> Newtype CResolvedType -> ClassFile
generateNewtype codeProfile moduleName javaPackageFn decl newtype_ =
  -- In java a newtype is just a single valueed struct
  generateStruct codeProfile moduleName javaPackageFn decl struct
  where
    struct = Struct {
      s_typeParams = n_typeParams newtype_,
      s_fields =
        [ Field {
           f_name = "value",
           f_type = n_typeExpr newtype_,
           f_default = n_default newtype_,
           f_annotations = mempty
           }
        ]
      }

generateUnion :: CodeGenProfile -> ModuleName -> (ModuleName -> JavaPackage) -> Decl CResolvedType -> Union CResolvedType -> ClassFile
generateUnion codeProfile moduleName javaPackageFn decl union =  execState gen state0
  where
    className = unreserveWord (d_name decl)
    state0 = classFile codeProfile moduleName javaPackageFn classDecl
    classDecl = "public class " <> className <> typeArgs
    isGeneric = length (u_typeParams union) > 0
    discVar = if cgp_hungarianNaming codeProfile then "mDisc" else "disc"
    valueVar = if cgp_hungarianNaming codeProfile then "mValue" else "value"
    typeArgs = case u_typeParams union of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    typecast fd from =
      if needsSuppressedCheckInCast (f_type (fd_field fd))
        then template "$1.<$2>cast($3)" [className,fd_boxedTypeExprStr fd,from]
        else template "($1) $2" [fd_boxedTypeExprStr fd,from]
    
    gen = do
      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (u_fields union)
      fieldDetail0 <- case fieldDetails of
        [] -> error "BUG: unions with no fields are illegal"
        (fd:_) -> return fd

      preventImport className
      for_ fieldDetails (\fd -> preventImport (fd_fieldName fd))
        
      objectsClass <- addImport "java.util" "Objects"

      -- Fields
      let modifiers = T.intercalate " " (["private"] <> if cgp_mutable codeProfile then [] else ["final"])
      addField (ctemplate "$1 Disc $2;" [modifiers,discVar])
      addField (ctemplate "$1 Object $2;" [modifiers,valueVar])

      -- Discriminator enum
      let terminators = replicate (length fieldDetails-1) "," <> [""]
          discdef =
            docStringComment (template "The $1 discriminator type." [className])
            <>
            cblock "public enum Disc" (
              mconcat [ctemplate "$1$2" [discriminatorName fd,term]
                      | (fd,term) <- zip fieldDetails terminators]
               )
      addMethod discdef

      -- constructors
      addMethod (cline "/* Constructors */")
      
      for_ fieldDetails $ \fd -> do
        let checkedv = if needsNullCheck fd then template "$1.requireNonNull(v)" [objectsClass] else "v"
            ctor = cblock (template "public static$1 $2 $3($4 v)" [leadSpace typeArgs, className, unionCtorName (fd_field fd), fd_typeExprStr fd]) (
              ctemplate "return new $1(Disc.$2, $3);" [className, discriminatorName fd, checkedv]
              )
            ctorvoid = cblock (template "public static$1 $2 $3()" [leadSpace typeArgs, className, unionCtorName (fd_field fd)]) (
              ctemplate "return new $1(Disc.$2, null);" [className, discriminatorName fd]
              )

        addMethod (if isVoidType (f_type (fd_field fd)) then ctorvoid else ctor)

      let ctorPrivate = cblock (template "private $1(Disc disc, Object value)" [className]) (
            ctemplate "this.$1 = disc;" [discVar]
            <>
            ctemplate "this.$1 = value;" [valueVar]
            )

          ctorDefault = cblock (template "public $1()" [className]) (
            ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fieldDetail0]
            <>
            ctemplate "this.$1 = $2;" [valueVar,fd_defValue fieldDetail0]
            )

          ctorCopy = cblock (template "public $1($1 other)" [className]) (
            ctemplate "this.$1 = other.$1;" [discVar]
            <>
            cblock (template "switch (other.$1)" [discVar]) (
              mconcat [
                ctemplate "case $1:" [discriminatorName fd]
                <>
                indent (
                  ctemplate "this.$1 = $2;" [valueVar,fd_copy fd (typecast fd ("other." <> valueVar))]
                  <>
                  cline "break;"
                  )
                | fd <- fieldDetails]
              )
            )

      when (not isGeneric) $ do
          addMethod ctorDefault
          addMethod ctorCopy
      addMethod $ ctorPrivate

      -- accessors
      addMethod (cline "/* Accessors */")

      addMethod $ cblock "public Disc getDisc()" (
        ctemplate "return $1;" [discVar]
        )

      for_ fieldDetails $ \fd -> do
        let getter = cblock (template "public $1 get$2()" [fd_typeExprStr fd, javaCapsFieldName (fd_field fd)]) (
              cblock (template "if ($1 == Disc.$2)" [discVar,discriminatorName fd]) (
                 ctemplate "return $1;" [typecast fd valueVar]
                 )
              <>
              cline "throw new IllegalStateException();"
              )

        when (not (isVoidType (f_type (fd_field fd)))) (addMethod getter)

      -- mutators
      addMethod (cline "/* Mutators */")

      when (cgp_mutable codeProfile) $ do 
        for_ fieldDetails $ \fd -> do
          let checkedv = if needsNullCheck fd then template "$1.requireNonNull(v)" [objectsClass] else "v"
              mtor = cblock (template "public void set$1($2 v)" [javaCapsFieldName (fd_field fd), fd_typeExprStr fd]) (
                ctemplate "this.$1 = $2;" [valueVar,checkedv]
                <>
                ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fd]
                )
              mtorvoid = cblock (template "public void set$1()" [javaCapsFieldName (fd_field fd)]) (
                ctemplate "this.$1 = null;" [valueVar]
                <>
                ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fd]
                )
          addMethod (if isVoidType (f_type (fd_field fd)) then mtorvoid else mtor)

      -- equals and hashcode
      addMethod (cline "/* Object level helpers */")

      addMethod $ coverride "public boolean equals(Object other0)" (
        cblock (template "if (!(other0 instanceof $1))"  [className]) (
          cline "return false;"
          )
        <>
        ctemplate "$1 other = ($1) other0;" [className]
        <>
        ctemplate "return $1 == other.$1 && $2.equals(other.$2);" [discVar,valueVar]
        )

      addMethod $ coverride "public int hashCode()" (
        ctemplate "return $1.hashCode() * 37 + $2.hashCode();" [discVar,valueVar]
        )

      -- cast helper
      let needCastHelper = (or [needsSuppressedCheckInCast (f_type (fd_field fd))| fd <- fieldDetails])
      when needCastHelper $ addMethod (
        cline "@SuppressWarnings(\"unchecked\")"
        <>
        cblock "private static <T> T cast(final Object o)" (
          cline "return (T) o;"
          )
        )

      -- factory
      factoryInterface <- addImport (cgp_runtimePackage codeProfile) "Factory"
      
      let factory =
            cblock1 (template "public static final $2<$1> FACTORY = new $2<$1>()" [className,factoryInterface]) (
              cblock (template "public $1 create()" [className]) (
                 ctemplate "return new $1();" [className]
              )
              <>
              cblock (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
            )

      let factoryg =
            cblock (template "public static$2 $3<$1$2> factory($4)" [className,leadSpace typeArgs,factoryInterface,factoryArgs]) (
              cblock1 (template "return new Factory<$1$2>()" [className,typeArgs]) (
                mconcat [ctemplate "final Factory<$1> $2 = $3;" [fd_boxedTypeExprStr fd,fd_fieldName fd,fd_factoryExprStr fd] | fd <- fieldDetails, not (immutableType (f_type (fd_field fd)))]
                <>
                cline ""
                <>
                cblock (template "public $1$2 create()" [className,typeArgs]) (
                  let val = if immutableType (f_type (fd_field fieldDetail0))
                            then fd_defValue fieldDetail0
                            else template "$1.create()" [fd_fieldName fieldDetail0]
                  in ctemplate "return new $1$2(Disc.$3,$4);" [className,typeArgs,discriminatorName fieldDetail0,val]
                )
                <>
                cline ""
                <>
                cblock (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
                  cblock (template "switch (other.$1)" [discVar]) (
                    mconcat [
                      ctemplate "case $1:" [discriminatorName fd]
                      <>
                      indent (
                        ctemplate "return new $1$2(other.$3,$4);"
                          [ className
                          , typeArgs
                          , discVar
                          , if immutableType (f_type (fd_field fd))
                              then template "other.$1" [valueVar]
                              else template "$1.create($2)" [fd_fieldName fd,typecast fd ("other." <>valueVar)]
                          ]
                        )
                      | fd <- fieldDetails]
                    )
                  <>
                  cline "throw new IllegalArgumentException();" 
                  )
                )
              )

          factoryArgs = commaSep [template "Factory<$1> $2" [arg,factoryTypeArg arg] | arg <- u_typeParams union]

      addMethod (cline "/* Factory for construction of generic values */")
      addMethod (if isGeneric then factoryg else factory)

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        generateUnionParcelable codeProfile decl union fieldDetails

