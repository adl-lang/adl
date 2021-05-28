{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java(
  generate,
  generateBatch,
  JavaFlags(..),
  CodeGenProfile(..),
  defaultCodeGenProfile,
  javaPackage,
  mkJavaPackageFn,
  JavaPackageFn,
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
import Data.Foldable(for_,fold)
import Data.List(intersperse,replicate,sort)
import Data.Maybe(isJust, catMaybes)
import Data.Monoid
import Data.String(IsString(..))
import Data.Traversable(for)
import System.FilePath

import ADL.Compiler.AST
import ADL.Utils.IndentedCode
import ADL.Compiler.Backends.Java.Internal
import ADL.Compiler.Backends.Java.Parcelable
import ADL.Compiler.Backends.BatchUtils
import ADL.Compiler.EIO
import ADL.Compiler.DataFiles
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Compiler.Utils
import ADL.Core.Value
import ADL.Core.Nullable(unNullable)
import ADL.Utils.FileDiff(dirContents)
import ADL.Utils.Format
import ADL.Adlc.Codegen.Java(JavaParams(..))
import ADL.Adlc.Codegen.Types(AdlSources, AdlTreeSource(..),OutputParams(..))

type JavaPackageFn = ModuleName -> JavaPackage

generateBatch :: FilePath -> JavaParams -> EIOT ()
generateBatch libDir params = do
  let log = batchLogFn (javaParams_verbose params)
  let mf = batchModuleFinder log (javaParams_sources params) (javaParams_mergeExts params)
  withBatchFileWriter log (javaParams_output params) $ \fileWriter -> do
    modulePaths <- mapM (\m -> findModule mf "batch" (moduleNameFromText m)) (javaParams_modules params) 
    (reqmods,allmods) <- loadAndCheckModules mf modulePaths
    let genmods = if (javaParams_generateTransitive params) then allmods else reqmods
    imports <- generateModules jf fileWriter genmods allmods
    when (jf_includeRuntime jf) $ do
      generateRuntime mf jf fileWriter imports
  where
    jf  = JavaFlags {
      jf_libDir = libDir,
      jf_package = javaPackage (javaParams_package params),
      jf_includeRuntime = (javaParams_includeRuntime params),
      jf_codeGenProfile = defaultCodeGenProfile {
        cgp_header = javaParams_headerComment params,
        cgp_mutable = True,
        cgp_maxLineLength = case unNullable (javaParams_maxLineLength params) of
           Nothing -> 10000
           (Just v) -> fromIntegral v,
        cgp_hungarianNaming = javaParams_hungarianNaming params,
        cgp_publicMembers = False,
        cgp_genericFactories = False,
        cgp_builder = True,
        cgp_parcelable = javaParams_parcellable params,
        cgp_runtimePackage = defaultRuntimePackage,
        cgp_supressWarnings = javaParams_suppressWarnings params
      }
    }

generate :: AdlFlags -> JavaFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af jf fileWriter modulePaths = catchAllExceptions  $ do
  let mf = moduleFinder af

  (reqmods,allmods) <- loadAndCheckModules mf modulePaths
  let genmods = if af_generateTransitive af then allmods else reqmods

  imports <- generateModules jf fileWriter genmods allmods
  when (jf_includeRuntime jf) $ do
    generateRuntime mf jf fileWriter imports

generateModules :: JavaFlags -> FileWriter -> [RModule] -> [RModule] -> EIOT (Set.Set JavaClass)
generateModules jf fileWriter genmods allmods = do
  let cgp = (jf_codeGenProfile jf)
      pkgfn = mkJavaPackageFn cgp allmods (jf_package jf)
  imports <- for genmods $ \mod -> do
    if generateCode (m_annotations mod)
      then generateModule jf fileWriter (const cgp) pkgfn mod
      else return Set.empty
  return (mconcat imports)

-- | Generate and write the java code for a single ADL module
-- The result value is the set of all java imports.
generateModule :: JavaFlags ->
                  FileWriter ->
                  (ScopedName -> CodeGenProfile) ->
                  JavaPackageFn ->
                  RModule ->
                  EIO T.Text (Set.Set JavaClass)
generateModule jf fileWriter mCodeGetProfile javaPackageFn m0 = do
  let moduleName = m_name m
      m = ( associateCustomTypes getCustomType moduleName
          . removeModuleTypedefs
          . expandModuleTypedefs
          ) m0
      decls = Map.elems (m_decls m)

  checkCustomSerializations m

  imports <- for decls $ \decl -> do
    let codeProfile = mCodeGetProfile (ScopedName moduleName (d_name decl))
        maxLineLength = Just (cgp_maxLineLength codeProfile)
        filePath = javaClassFilePath (javaClass (javaPackageFn moduleName) (d_name decl))
        generateType = case d_customType decl of
          Nothing ->  generateCode (d_annotations decl)
          (Just ct) -> ct_generateType ct

    if generateType
      then do
        classFile <- case d_type decl of
          (Decl_Struct s) -> return (generateStruct codeProfile moduleName javaPackageFn decl s)
          (Decl_Union u)
            | isEnumeration u -> return (generateEnum codeProfile moduleName javaPackageFn decl u)
            | otherwise       -> return (generateUnion codeProfile moduleName javaPackageFn decl u)
          (Decl_Newtype n) -> return (generateNewtype codeProfile moduleName javaPackageFn decl n)
          (Decl_Typedef _) -> eioError "BUG: typedefs should have been eliminated"
        let lines = codeText maxLineLength (classFileCode classFile)
            imports = Set.fromList ([javaClass pkg cls| (cls,Just pkg) <- Map.toList (cf_imports classFile)])
        liftIO $ fileWriter filePath (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" lines <> "\n")))
        return imports
      else do
        return mempty
  return (mconcat imports)

generateStruct :: CodeGenProfile -> ModuleName -> JavaPackageFn -> CDecl -> Struct CResolvedType -> ClassFile
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
      fieldDetails <- generateCoreStruct codeProfile moduleName javaPackageFn decl struct

      -- Json
      generateStructJson codeProfile decl struct fieldDetails

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        generateStructParcelable codeProfile decl struct fieldDetails

generateNewtype :: CodeGenProfile -> ModuleName -> JavaPackageFn -> CDecl -> Newtype CResolvedType -> ClassFile
generateNewtype codeProfile moduleName javaPackageFn decl newtype_ = execState gen state0
  where
    className = unreserveWord (d_name decl)
    classDecl = "public class " <> className <> typeArgs
    state0 = classFile codeProfile moduleName javaPackageFn classDecl
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"

    -- In java a newtype is just a single valued struct (with special serialisation)
    struct = Struct {
      s_typeParams = n_typeParams newtype_,
      s_fields =
        [ Field {
           f_name = "value",
           f_serializedName = "UNUSED",
           f_type = n_typeExpr newtype_,
           f_default = n_default newtype_,
           f_annotations = mempty
           }
        ]
      }

    gen = do
      fieldDetails <- generateCoreStruct codeProfile moduleName javaPackageFn decl struct

      -- Json
      generateNewtypeJson codeProfile decl newtype_ (fd_memberVarName (head fieldDetails))

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        generateStructParcelable codeProfile decl struct fieldDetails

generateCoreStruct :: CodeGenProfile -> ModuleName -> JavaPackageFn
                   -> CDecl -> Struct CResolvedType -> CState [FieldDetails]
generateCoreStruct codeProfile moduleName javaPackageFn decl struct =  gen
  where
    className = unreserveWord (d_name decl)
    state0 = classFile codeProfile moduleName javaPackageFn classDecl
    isEmpty = null (s_fields struct)
    classDecl = "public class " <> className <> typeArgs
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    gen = do
      addImport (javaClass (javaPackageFn moduleName) className)

      setDocString (generateDocString (d_annotations decl))

      fieldDetails <- mapM genFieldDetails (s_fields struct)
      let fieldsWithDefaults = filter (\fd -> fd_hasDefault fd && not (isTypeToken fd)) fieldDetails
          hasDefaults = not (null fieldsWithDefaults)
      for_ fieldDetails (\fd -> preventImport (fd_memberVarName fd))
      for_ fieldDetails (\fd -> preventImport (fd_varName fd))

      objectsClass <- addImport "java.util.Objects"

      -- Fields
      for_ fieldDetails $ \fd -> do
        let modifiers =
             (if cgp_publicMembers codeProfile then ["public"] else ["private"])
             <>
             (if cgp_mutable codeProfile then [] else ["final"])
        addField (ctemplate "$1 $2 $3;" [T.intercalate " " modifiers,fd_typeExprStr fd,fd_memberVarName fd])

      -- Constructors
      let ctorAllArgs = T.intercalate ", " [fd_typeExprStr fd <> " " <> fd_varName fd | fd <- fieldDetails]
          ctorReqArgs = T.intercalate ", " [fd_typeExprStr fd <> " " <> fd_varName fd | fd <- fieldDetails, not (fd_hasDefault fd)]
          isGeneric = length (s_typeParams struct) > 0

          ctor1 =
            cblock (template "public $1($2)" [className,ctorAllArgs]) (
              clineN [
                if needsNullCheck fd
                  then template "this.$1 = $2.requireNonNull($3);" [fd_memberVarName fd, objectsClass, fd_varName fd]
                  else template "this.$1 = $2;" [fd_memberVarName fd, fd_varName fd]
                | fd <- fieldDetails]
            )

          ctor2 =
            cblock (template "public $1($2)" [className, ctorReqArgs]) (
              clineN
                [template "this.$1 = $2;"
                  [ fd_memberVarName fd,
                    if fd_hasDefault fd
                       then template "$1()" [fd_defFnName fd]
                       else if needsNullCheck fd
                            then template "$1.requireNonNull($2)" [objectsClass, fd_varName fd]
                            else template "$1" [fd_varName fd]
                  ]
                | fd <- fieldDetails]
            )

          ctor3 =
            cblock (template "public $1($2 other)" [className, className <> typeArgs]) (
              mconcat [ let n = fd_memberVarName fd in ctemplate "this.$1 = $2;" [n,fd_copy fd ("other." <>n)]
                      | fd <- fieldDetails ]
            )

      addMethod (cline "/* Constructors */")

      addMethod ctor1
      when (not isGeneric && hasDefaults) (addMethod ctor2)
      when (not isGeneric) (addMethod ctor3)

      -- Default static fns
      when (not (null fieldsWithDefaults)) $ do
        (addMethod (cline "/* Field defaults */"))
      for_ fieldsWithDefaults $ \fd -> do
        let deffn =
              cblock (template "public static $1$2 $3()" [tparams, fd_typeExprStr fd,fd_defFnName fd]) (
                ctemplate "return $1;" [fd_defValue fd]
              )

            tparams = case Set.toList (unboundTvars (f_type (fd_field fd))) of
              [] -> ""
              tvars -> "<" <> T.intercalate "," tvars <> "> "

            unboundTvars :: TypeExprRT c -> Set.Set Ident
            unboundTvars texpr =  foldMap rtVar texpr
            rtVar (RT_Param i) = Set.singleton i
            rtVar _ = Set.empty

        addMethod deffn

      -- Getters/Setters
      when (not isEmpty) (addMethod (cline "/* Accessors and mutators */"))

      when (not (cgp_publicMembers codeProfile)) $ do
        for_ fieldDetails $ \fd -> do
          let getter =
                cblock (template "public $1 $2()" [fd_typeExprStr fd,fd_accessorName fd]) (
                  ctemplate "return $1;" [fd_memberVarName fd]
                )
              setter =
                cblock (template "public $1 $2($3 $4)" [className <> typeArgs, fd_mutatorName fd,fd_typeExprStr fd, fd_varName fd]) (
                  ( if needsNullCheck fd
                      then ctemplate "this.$1 = $2.requireNonNull($3);" [fd_memberVarName fd,objectsClass,fd_varName fd]
                      else ctemplate "this.$1 = $2;" [fd_memberVarName fd,fd_varName fd]
                  )
                  <> cline "return this;"
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
            ctemplate "$1 other = ($1) other0;" [withUnboundedTypeArgs className (s_typeParams struct)]
            <>
            cline "return"
            <>
            let terminators = replicate (length fieldDetails-1) " &&" <> [";"]
                tests = [cline (fd_equals fd (fd_memberVarName fd) ("other." <> fd_memberVarName fd) <> term)
                        | (fd,term) <- zip fieldDetails terminators]
            in  indent (mconcat tests)
            )
          equalsEmpty = coverride "public boolean equals(Object other)" (
            ctemplate "return other instanceof $1;" [className]
            )
      addMethod (if isEmpty then equalsEmpty else equals)

      addMethod $ coverride "public int hashCode()" (
        cline "int _result = 1;"
        <>
        mconcat [ctemplate "_result = _result * 37 + $1;" [fd_hashcode fd (fd_memberVarName fd)] | fd <- fieldDetails]
        <>
        cline "return _result;"
        )

      factoryInterface <- addImport (javaClass (cgp_runtimePackage codeProfile) "Factory")
      typeExprMethodCode <- genTypeExprMethod codeProfile moduleName decl

      --  if enabled and we have more than one field
      -- and we don't contain any typetokens
      when (cgp_builder codeProfile && length fieldDetails > 1 && not (any isTypeToken fieldDetails)) $ do
        buildersI <- addImport (javaClass (cgp_runtimePackage codeProfile) "Builders")
        let initialValue fd = if hasDefault fd then fd_defValue fd else "null"
            hasDefault fd = case f_default (fd_field fd) of
              Nothing -> False
              Just _ -> True

            setter fd = cblock (template "public Builder$1 $2($3 $4)"
                                [typeArgs, fd_mutatorName fd, fd_boxedTypeExprStr fd, fd_varName fd])
              (  ctemplate "this.$1 = $2.requireNonNull($3);" [fd_memberVarName fd,objectsClass,fd_varName fd]
              <> cline "return this;"
              )

            checkField fd = ctemplate "$1.checkFieldInitialized(\"$2\", \"$3\", $3);"
                [buildersI,className,fd_memberVarName fd]

            builder = cblock (template "public static class Builder$1" [typeArgs])
              (  mconcat [ctemplate "private $1 $2;" [fd_boxedTypeExprStr fd,fd_memberVarName fd]
                         | fd <- fieldDetails]
              <> cline ""
              <> cblock "public Builder()"
                (  mconcat [ctemplate "this.$1 = $2;" [fd_memberVarName fd, initialValue fd]
                           | fd <- fieldDetails]
                )
              <> mconcat [cline "" <> setter fd | fd <- fieldDetails]
              <> cline ""
              <> cblock (template "public $1$2 create()" [className, typeArgs])
                (  mconcat [checkField fd | fd <- fieldDetails, not (hasDefault fd)]
                <> ctemplate "return new $1$2($3);" [className, typeArgs, T.intercalate ", " (map fd_memberVarName fieldDetails)]
                )
              )

        addMethod (cline "/* Builder */")
        addMethod builder

      -- factory
      let factory =
            cblock1 (template "public static final $2<$1> FACTORY = new $2<$1>()" [className,factoryInterface]) (
              coverride (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
              <>
              cline ""
              <>
              typeExprMethodCode
              <>
              coverride (template "public JsonBinding<$1> jsonBinding()" [className]) (
                ctemplate "return $1.jsonBinding();" [className]
              )
            )

      let factoryg lazyC =
            cblock (template "public static $2 $3<$1$2> factory($4)" [className,typeArgs,factoryInterface,factoryArgs]) (
              cblock1 (template "return new $1<$2$3>()" [factoryInterface,className,typeArgs]) (
                mconcat [ctemplate "final $1<$2<$3>> $4 = new $1<>(() -> $5);"
                                   [lazyC,factoryInterface,fd_boxedTypeExprStr fd,fd_varName fd,fd_factoryExprStr fd]
                        | fd <- fieldDetails]
                <>
                cline ""
                <>
                coverride (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
                   ctemplate "return new $1$2(" [className,typeArgs]
                   <>
                   indent (clineN (addTerminators "," "," ""  ctor2Args) <> cline ");")
                   )
                <>
                cline ""
                <>
                typeExprMethodCode
                <>
                cline ""
                <>
                coverride (template "public JsonBinding<$1$2> jsonBinding()" [className,typeArgs]) (
                  ctemplate "return $1.jsonBinding($2);" [className,jsonBindingArgs]
                )
              )
            )

          factoryArgs = commaSep [template "$1<$2> $3" [factoryInterface,arg,factoryTypeArg arg] | arg <- s_typeParams struct]
          jsonBindingArgs = commaSep [template "$1.jsonBinding()" [factoryTypeArg arg] | arg <- s_typeParams struct]
          ctor1Args = [case f_default (fd_field fd) of
                        Nothing -> template "$1.get().create()" [fd_varName fd]
                        (Just _) -> replaceTypeVarsWithFactoryMethods (fd_defValue fd)
                      | fd <-fieldDetails]
          ctor2Args = [if immutableType (f_type (fd_field fd))
                       then template "other.$1" [fd_accessExpr fd]
                       else template "$1.get().create(other.$2)" [fd_varName fd,fd_accessExpr fd]
                      | fd <- fieldDetails]

      addMethod (cline "/* Factory for construction of generic values */")

      if isGeneric
        then do
          lazyC <- addImport (javaClass (cgp_runtimePackage codeProfile) "Lazy")
          addMethod (factoryg lazyC)
        else do
          addMethod factory

      return fieldDetails

data UnionType = AllVoids | NoVoids | Mixed

generateUnion :: CodeGenProfile -> ModuleName -> JavaPackageFn -> CDecl -> Union CResolvedType -> ClassFile
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

    unionType = if and voidTypes then AllVoids else if or voidTypes then Mixed else NoVoids
      where
        voidTypes = [isVoidType (f_type f) | f <- u_fields union]

    gen = do
      addImport (javaClass (javaPackageFn moduleName) className)
      preventImport discVar
      preventImport valueVar

      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (u_fields union)
      fieldDetail0 <- case fieldDetails of
        [] -> error "BUG: unions with no fields are illegal"
        (fd:_) -> return fd

      for_ fieldDetails (\fd -> preventImport (fd_memberVarName fd))
      for_ fieldDetails (\fd -> preventImport (fd_varName fd))

      objectsClass <- addImport "java.util.Objects"

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
            ctor = cblock (template "public static$1 $2$3 $4($5 v)" [leadSpace typeArgs, className, typeArgs, fd_unionCtorName fd, fd_typeExprStr fd]) (
              ctemplate "return new $1$2(Disc.$3, $4);" [className, typeArgs, discriminatorName fd, checkedv]
              )
            ctorvoid = cblock (template "public static$1 $2$3 $4()" [leadSpace typeArgs, className, typeArgs, fd_unionCtorName fd]) (
              ctemplate "return new $1$2(Disc.$3, null);" [className, typeArgs, discriminatorName fd]
              )

        addMethod (if isVoidType (f_type (fd_field fd)) then ctorvoid else ctor)

      let ctorPrivate = cblock (template "private $1(Disc disc, Object value)" [className]) (
            ctemplate "this.$1 = disc;" [discVar]
            <>
            ctemplate "this.$1 = value;" [valueVar]
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
        ctemplate "$1 other = ($1) other0;" [withUnboundedTypeArgs className (u_typeParams union)]
        <>
        case unionType of
          NoVoids -> ctemplate "return $1 == other.$1 && $2.equals(other.$2);" [discVar,valueVar]
          AllVoids -> ctemplate "return $1 == other.$1;" [discVar]
          Mixed ->
            cblock (template "switch ($1)" [discVar]) (
              mconcat [
                ctemplate "case $1:" [discriminatorName fd]
                <>
                indent (
                  if isVoidType (f_type (fd_field fd))
                     then ctemplate "return $1 == other.$1;" [discVar]
                     else ctemplate "return $1 == other.$1 && $2.equals(other.$2);" [discVar,valueVar]
                  )
                | fd <- fieldDetails]
            )
            <>
            cline "throw new IllegalStateException();"
       )

      addMethod $ coverride "public int hashCode()" (
        case unionType of
          NoVoids -> ctemplate "return $1.hashCode() * 37 + $2.hashCode();" [discVar,valueVar]
          AllVoids -> ctemplate "return $1.hashCode();" [discVar]
          Mixed ->
            cblock (template "switch ($1)" [discVar]) (
              mconcat [
                ctemplate "case $1:" [discriminatorName fd]
                <>
                indent (
                  if isVoidType (f_type (fd_field fd))
                     then ctemplate "return $1.hashCode();" [discVar]
                     else ctemplate "return $1.hashCode() * 37 + $2.hashCode();" [discVar,valueVar]
                  )
                | fd <- fieldDetails]
            )
            <>
            cline "throw new IllegalStateException();"
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
      factoryInterface <- addImport (javaClass (cgp_runtimePackage codeProfile) "Factory")
      typeExprMethodCode <- genTypeExprMethod codeProfile moduleName decl

      let factory =
            cblock1 (template "public static final $2<$1> FACTORY = new $2<$1>()" [className,factoryInterface]) (
              coverride (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
              <>
              cline ""
              <>
              typeExprMethodCode
              <>
              cline ""
              <>
              coverride (template "public JsonBinding<$1> jsonBinding()" [className]) (
                ctemplate "return $1.jsonBinding();" [className]
              )
            )

      let factoryg lazyC =
            cblock (template "public static$2 $3<$1$2> factory($4)" [className,leadSpace typeArgs,factoryInterface,factoryArgs]) (
              cblock1 (template "return new Factory<$1$2>()" [className,typeArgs]) (
                mconcat [ctemplate "final $1<Factory<$2>> $3 = new $1<>(() -> $4);"
                                   [lazyC,fd_boxedTypeExprStr fd,fd_varName fd,fd_factoryExprStr fd] | fd <- fieldDetails]
                <>
                cline ""
                <>
                cline ""
                <>
                coverride (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
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
                              else template "$1.get().create($2)" [fd_varName fd,typecast fd ("other." <>valueVar)]
                          ]
                        )
                      | fd <- fieldDetails]
                    )
                    <>
                    cline "throw new IllegalArgumentException();"
                  )
                  <>
                  cline ""
                  <>
                  typeExprMethodCode
                  <> 
                  cline ""
                  <>
                  coverride (template "public JsonBinding<$1$2> jsonBinding()" [className,typeArgs]) (
                    ctemplate "return $1.jsonBinding($2);" [className,jsonBindingArgs]
                  )
                )
              )

          factoryArgs = commaSep [template "Factory<$1> $2" [arg,factoryTypeArg arg] | arg <- u_typeParams union]
          jsonBindingArgs = commaSep [template "$1.jsonBinding()" [factoryTypeArg arg] | arg <- u_typeParams union]

      addMethod (cline "/* Factory for construction of generic values */")
      if isGeneric
        then do
          lazyC <- addImport (javaClass (cgp_runtimePackage codeProfile) "Lazy")
          addMethod (factoryg lazyC)
        else do
          addMethod factory

      -- Json
      generateUnionJson codeProfile decl union fieldDetails

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        generateUnionParcelable codeProfile decl union fieldDetails

generateEnum :: CodeGenProfile -> ModuleName -> JavaPackageFn -> CDecl -> Union CResolvedType -> ClassFile
generateEnum codeProfile moduleName javaPackageFn decl union = execState gen state0
  where
    className = unreserveWord (d_name decl)
    classDecl = "public enum " <> className
    state0 = classFile codeProfile moduleName javaPackageFn classDecl

    gen = do
      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (u_fields union)
      fieldDetail0 <- case fieldDetails of
        [] -> error "BUG: unions with no fields are illegal"
        (fd:_) -> return fd
      factoryInterface <- addImport (javaClass (cgp_runtimePackage codeProfile) "Factory")

      let terminators = replicate (length fieldDetails-1) "," <> [";"]
      mapM_ addField [ctemplate "$1$2" [discriminatorName fd,term] | (fd,term) <- zip fieldDetails terminators]

      addMethod $ coverride "public String toString()" (
        cblock "switch(this)" (
           mconcat [ctemplate "case $1: return \"$2\";" [discriminatorName fd, fd_serializedName fd] | fd <- fieldDetails]
           )
        <> cline "throw new IllegalArgumentException();"
        )

      addMethod $ cblock (template "public static $1 fromString(String s)" [className]) (
        mconcat [cblock (template "if (s.equals(\"$1\"))" [fd_serializedName fd]) (
                    ctemplate "return $1;" [discriminatorName fd]
                    )
                | fd <- fieldDetails ]
        <> cline "throw new IllegalArgumentException(\"illegal value: \" + s);"
        )

      typeExprMethodCode <- genTypeExprMethod codeProfile moduleName decl

      addMethod $ cblock1 (template "public static final $2<$1> FACTORY = new $2<$1>()" [className,factoryInterface])
        ( coverride (template "public $1 create($1 other)" [className])
            ( cline "return other;"
            )
        <> cline ""
        <> typeExprMethodCode
        <>
        coverride (template "public JsonBinding<$1> jsonBinding()" [className]) (
          ctemplate "return $1.jsonBinding();" [className]
        )
        )

      -- Json
      generateEnumJson codeProfile decl union fieldDetails

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        error "Unimplemented: Parcellable for enums"


generateRuntime:: ModuleFinder -> JavaFlags -> FileWriter -> Set.Set JavaClass -> EIOT ()
generateRuntime mf jf fileWriter imports1 = do
    sysmods <- loadSystemModules
    imports2 <- generateModules jf fileWriter sysmods sysmods
    generateRuntime0 jf fileWriter (imports1 <> imports2)
  where
    loadSystemModules :: EIOT [RModule]
    loadSystemModules =  do
      sysModulePaths <- mapM (findModule mf "runtime") sysModules
      fmap fst (loadAndCheckModules mf sysModulePaths)

generateRuntime0 :: JavaFlags -> FileWriter -> Set.Set JavaClass -> EIOT ()
generateRuntime0 jf fileWriter imports = liftIO $ do
    files <- dirContents runtimedir
    for_ files $ \inpath -> do
      let cls = javaClass rtpackage (T.pack (dropExtensions (takeFileName inpath)))
          toGenerate
            =  imports
            <> (if Set.member (javaClass rtpackage "JsonBinding") imports
                  then Set.fromList
                    [ javaClass rtpackage "JsonParseException"
                    , javaClass rtpackage "JsonHelpers"
                    , javaClass rtpackage "DynamicHelpers"
                    , javaClass rtpackage "TypeToken"
                    ]
                  else mempty
               )
            <> (Set.fromList
                  [ javaClass rtpackage "ByteArray"
                  , javaClass rtpackage "Factory"
                  , javaClass rtpackage "Factories"
                  ]
               )
      when (Set.member cls toGenerate) $ do
        content <- LBS.readFile (runtimedir </> inpath)
        fileWriter (javaClassFilePath cls) (adjustContent content)
  where
    runtimedir =  javaRuntimeDir (jf_libDir jf)
    rtpackage = cgp_runtimePackage (jf_codeGenProfile jf)

    adjustContent :: LBS.ByteString -> LBS.ByteString
    adjustContent origLBS = LBS.fromStrict (T.encodeUtf8 newT)
      where origT = T.decodeUtf8 (LBS.toStrict origLBS)
            newT = T.replace (genJavaPackage defaultRuntimePackage) (genJavaPackage rtpackage)
                 $ origT

-- Use the default output package and any
-- @JavaPackage annotation to create a mapping from
-- adl module names to java packages

mkJavaPackageFn :: CodeGenProfile -> [RModule] -> JavaPackage -> JavaPackageFn
mkJavaPackageFn cgp mods defJavaPackage = \modName -> case Map.lookup modName packageMap of
  Nothing -> defJavaPackage <> JavaPackage (unModuleName modName)
  (Just pkg) -> pkg
 where
   packageMap = mconcat (map getCustomJavaPackage mods)

   getCustomJavaPackage :: RModule -> Map.Map ModuleName JavaPackage
   getCustomJavaPackage mod = case Map.lookup snJavaPackage (m_annotations mod) of
     Just (_,JSON.String s) -> Map.singleton (m_name mod) (fixRuntimePackage cgp (javaPackage s))
     _ -> Map.empty


genTypeExprMethod :: CodeGenProfile -> ModuleName -> CDecl -> CState Code
genTypeExprMethod cgp moduleName decl = do
  let adlastPackage = getAdlAstPackage cgp
      className = unreserveWord (d_name decl)
  typeExprI <- addImport (javaClass adlastPackage "TypeExpr")
  typeRefI <- addImport (javaClass adlastPackage "TypeRef")
  scopedNameI <- addImport (javaClass adlastPackage "ScopedName")
  arrayListI <- addImport "java.util.ArrayList"
  return $ coverride  (template "public $1 typeExpr()" [typeExprI])
    (  ctemplate "$1 scopedName = new $1(\"$2\", \"$3\");" [scopedNameI,formatText moduleName,className]
       <> ctemplate "$1<$2> params = new $1<>();" [arrayListI,typeExprI]
       <> (mconcat [ ctemplate "params.add(factory$1.typeExpr());" [tparam] | tparam <- getTypeParams (d_type decl)])
       <> ctemplate "return new $1($2.reference(scopedName), params);" [typeExprI,typeRefI]
    )

isTypeToken :: FieldDetails -> Bool
isTypeToken fd = case f_type (fd_field fd) of
  TypeExpr (RT_Primitive P_TypeToken) _ -> True
  _ -> False

generateCode :: Annotations t -> Bool
generateCode annotations = case Map.lookup snJavaGenerate annotations of
  Just (_,JSON.Bool gen) -> gen
  _ -> True

getAdlAstPackage :: CodeGenProfile -> JavaPackage
getAdlAstPackage cgp = cgp_runtimePackage cgp <> JavaPackage ["sys","adlast"]

snJavaPackage :: ScopedName
snJavaPackage = ScopedName (ModuleName ["adlc","config","java"]) "JavaPackage"

snJavaGenerate :: ScopedName
snJavaGenerate = ScopedName (ModuleName ["adlc","config","java"]) "JavaGenerate"

sysModules :: [ModuleName]
sysModules = [
  ModuleName ["sys", "types"],
  ModuleName ["sys", "dynamic"],
  ModuleName ["sys", "adlast"]
  ]
