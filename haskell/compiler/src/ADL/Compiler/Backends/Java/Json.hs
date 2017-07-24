{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Java.Json(
  generateStructJson,
  generateNewtypeJson,
  generateUnionJson,
  generateEnumJson
  ) where

import Control.Monad(when)
import Data.Foldable(for_,fold)
import Data.Maybe(isJust)
import Data.Monoid
import Data.String(IsString(..))
import Data.Traversable(for)

import qualified Data.Text as T

import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Java.Internal
import ADL.Utils.IndentedCode
import ADL.Utils.Format

generateStructJson :: CodeGenProfile -> CDecl -> Struct CResolvedType -> [FieldDetails] -> CState ()
generateStructJson cgp decl struct fieldDetails = do
  let typeArgs = case s_typeParams struct of
        [] -> ""
        args -> "<" <> commaSep (map unreserveWord args) <> ">"
      className = unreserveWord (d_name decl) <> typeArgs

  factoryI <- addImport (javaClass (cgp_runtimePackage cgp) "Factory")
  lazyC <- addImport (javaClass (cgp_runtimePackage cgp) "Lazy")
  jsonBindingI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBinding")
  jsonBindingsI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBindings")
  jsonElementI <- addImport "com.google.gson.JsonElement"
  jsonObjectI <- addImport "com.google.gson.JsonObject"
  jsonBindings <- mapM (genJsonBindingExpr cgp . f_type . fd_field) fieldDetails

  let bindingArgs = commaSep [template "$1<$2> $3" [jsonBindingI,arg,"binding" <> arg] | arg <- s_typeParams struct]

  let factory =
        cblock (template "public static$1 $2<$3> jsonBinding($4)" [typeArgs,jsonBindingI,className,bindingArgs]) (
              clineN
                [ template "final $1<$2<$3>> $4 = new $1<>(() -> $5);" [lazyC,jsonBindingI,fd_boxedTypeExprStr fd,fd_varName fd,binding]
                | (fd,binding) <- zip fieldDetails jsonBindings]
              <>
              clineN
                [ template "final $1<$2> factory$2 = binding$2.factory();" [factoryI,typeParam]
                | typeParam <- s_typeParams struct]
              <>
              case s_typeParams struct of
                [] -> ctemplate "final $1<$2> _factory = FACTORY;" [factoryI,className]
                tparams -> ctemplate "final $1<$2> _factory = factory($3);" [factoryI,className,commaSep [template "binding$1.factory()" [i] | i <-tparams ]]
              <>
              cline ""
              <>
              cblock1 (template "return new $1<$2>()" [jsonBindingI,className]) (
                coverride (template "public $1<$2> factory()" [factoryI,className]) (
                  cline "return _factory;"
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 toJson($2 _value)" [jsonElementI,className]) (
                  ctemplate "$1 _result = new $1();" [jsonObjectI]
                  <>
                  clineN
                    [ template "_result.add(\"$1\", $2.get().toJson(_value.$3));" [fd_serializedName fd,fd_varName fd,fd_memberVarName fd]
                    | fd <- fieldDetails]
                  <>
                  cline "return _result;"
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 fromJson($2 _json)" [className,jsonElementI]) (
                  ctemplate "$1 _obj = $2.objectFromJson(_json);" [jsonObjectI,jsonBindingsI]
                  <>
                  ( ctemplate "return new $1(" [className]
                    <>
                    indent (
                      let terminators = replicate (length fieldDetails-1) "," <> [""]
                          requiredField fd terminator =
                            template "$2.fieldFromJson(_obj, \"$1\", $3.get())$4"
                                     [fd_serializedName fd,jsonBindingsI, fd_varName fd,terminator]
                          optionalField fd terminator =
                            template "_obj.has(\"$1\") ? $2.fieldFromJson(_obj, \"$1\", $3.get()) : $4$5"
                                     [fd_serializedName fd,jsonBindingsI, fd_varName fd,fd_defValue fd,terminator]
                      in
                      clineN
                      [ if isJust (f_default (fd_field fd))
                          then optionalField fd terminator
                          else requiredField fd terminator
                      | (fd,terminator) <- zip fieldDetails terminators]
                      )
                    <>
                    cline ");"
                    )
                  )
                )
              )

  addMethod (cline "/* Json serialization */")
  addMethod factory

generateNewtypeJson :: CodeGenProfile -> CDecl -> Newtype CResolvedType -> Ident -> CState ()
generateNewtypeJson cgp decl newtype_ memberVarName = do
  let typeArgs = case n_typeParams newtype_ of
        [] -> ""
        args -> "<" <> commaSep (map unreserveWord args) <> ">"
      className = unreserveWord (d_name decl) <> typeArgs

  factoryI <- addImport (javaClass (cgp_runtimePackage cgp) "Factory")
  jsonBindingI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBinding")
  jsonElementI <- addImport "com.google.gson.JsonElement"
  jsonObjectI <- addImport "com.google.gson.JsonObject"
  jsonBinding <- genJsonBindingExpr cgp (n_typeExpr newtype_)
  boxedTypeExprStr <- genTypeExprB TypeBoxed (n_typeExpr newtype_)

  let bindingArgs = commaSep [template "$1<$2> $3" [jsonBindingI,arg,"binding" <> arg] | arg <- n_typeParams newtype_]

  let factory =
        cblock (template "public static$1 $2<$3> jsonBinding($4)" [typeArgs,jsonBindingI,className,bindingArgs]) (
              ctemplate "final $1<$2> _binding = $3;" [jsonBindingI,boxedTypeExprStr,jsonBinding]
              <>
              case n_typeParams newtype_ of
                [] -> ctemplate "final $1<$2> _factory = FACTORY;" [factoryI,className]
                tparams -> ctemplate "final $1<$2> _factory = factory($3);" [factoryI,className,commaSep [template "binding$1.factory()" [i] | i <-tparams ]]
              <>
              cline ""
              <>
              cblock1 (template "return new $1<$2>()" [jsonBindingI,className]) (
                coverride (template "public $1<$2> factory()" [factoryI,className]) (
                  cline "return _factory;"
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 toJson($2 _value)" [jsonElementI,className]) (
                  ctemplate "return _binding.toJson(_value.$1);" [memberVarName]
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 fromJson($2 _json)" [className,jsonElementI]) (
                  ctemplate "return new $1(_binding.fromJson(_json));" [className]
                  )
                )
              )

  addMethod (cline "/* Json serialization */")
  addMethod factory

generateUnionJson :: CodeGenProfile -> CDecl -> Union CResolvedType -> [FieldDetails] -> CState ()
generateUnionJson cgp decl union fieldDetails = do
  let typeArgs = case u_typeParams union of
        [] -> ""
        args -> "<" <> commaSep (map unreserveWord args) <> ">"
      className0 = unreserveWord (d_name decl)
      className = className0 <> typeArgs

  factoryI <- addImport (javaClass (cgp_runtimePackage cgp) "Factory")
  lazyC <- addImport (javaClass (cgp_runtimePackage cgp) "Lazy")
  jsonBindingI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBinding")
  jsonBindingsI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBindings")
  jsonElementI <- addImport "com.google.gson.JsonElement"
  jsonParseExceptionI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonParseException")
  jsonBindings <- mapM (genJsonBindingExpr cgp . f_type . fd_field) fieldDetails

  let bindingArgs = commaSep [template "$1<$2> $3" [jsonBindingI,arg,"binding" <> arg] | arg <- u_typeParams union]

  let factory =
        cblock (template "public static$1 $2<$3> jsonBinding($4)" [typeArgs,jsonBindingI,className,bindingArgs]) (
              clineN
                [ template "final $1<$2<$3>> $4 = new $1<>(() -> $5);" [lazyC,jsonBindingI,fd_boxedTypeExprStr fd,fd_varName fd,binding]
                | (fd,binding) <- zip fieldDetails jsonBindings]
              <>
              clineN
                [ template "final $1<$2> factory$2 = binding$2.factory();" [factoryI,typeParam]
                | typeParam <- u_typeParams union]
              <>
              case u_typeParams union of
                [] -> ctemplate "final $1<$2> _factory = FACTORY;" [factoryI,className]
                tparams -> ctemplate "final $1<$2> _factory = factory($3);" [factoryI,className,commaSep [template "binding$1.factory()" [i] | i <-tparams ]]
              <>
              cline ""
              <>
              cblock1 (template "return new $1<$2>()" [jsonBindingI,className]) (
                coverride (template "public $1<$2> factory()" [factoryI,className]) (
                  cline "return _factory;"
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 toJson($2 _value)" [jsonElementI,className]) (
                  cblock "switch (_value.getDisc())" (
                    mconcat [
                       ctemplate "case $1:" [discriminatorName fd]
                       <>
                       indent (
                         if isVoidType (f_type (fd_field fd))
                         then ctemplate "return $1.unionToJson(\"$2\", null, null);"
                                        [jsonBindingsI, fd_serializedName fd]
                         else ctemplate "return $1.unionToJson(\"$2\", _value.$3, $4.get());"
                                        [jsonBindingsI, fd_serializedName fd, fd_accessExpr fd, fd_varName fd]
                       )
                       | fd <- fieldDetails ]
                  )
                  <>
                  cline "return null;"
                  )
                <>
                cline ""
                <>
                coverride (template "public $1 fromJson($2 _json)" [className,jsonElementI]) (
                  ctemplate "String _key = $1.unionNameFromJson(_json);" [jsonBindingsI]
                  <>
                  let returnStatements = [
                        if isVoidType (f_type (fd_field fd))
                        then ctemplate "return $1.$2$3();" [className0,typeArgs,fd_unionCtorName fd]
                        else ctemplate "return $1.$2$3($4.unionValueFromJson(_json, $5.get()));" [className0,typeArgs,fd_unionCtorName fd, jsonBindingsI, fd_varName fd]
                        | fd <- fieldDetails]
                  in ctemplate "if (_key.equals(\"$1\")) {" [fd_serializedName (head fieldDetails)]
                       <>
                       indent (head returnStatements)
                       <>
                       mconcat [
                         cline "}"
                         <>
                         ctemplate "else if (_key.equals(\"$1\")) {" [fd_serializedName fd]
                         <>
                         indent returnCase
                         | (fd,returnCase) <- zip (tail fieldDetails) (tail returnStatements)]
                       <>
                       cline "}"
                  <>
                  ctemplate "throw new $1(\"Invalid discriminator \" + _key + \" for union $2\");" [jsonParseExceptionI,className]
                  )
                )
              )

  addMethod (cline "/* Json serialization */")
  addMethod factory

generateEnumJson :: CodeGenProfile -> CDecl -> Union CResolvedType -> [FieldDetails] -> CState ()
generateEnumJson cgp decl union fieldDetails = do
  factoryI <- addImport (javaClass (cgp_runtimePackage cgp) "Factory")
  jsonBindingI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBinding")
  jsonElementI <- addImport "com.google.gson.JsonElement"
  jsonPrimitiveI <- addImport "com.google.gson.JsonPrimitive"
  jsonParseExceptionI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonParseException")

  let className = unreserveWord (d_name decl)
      factory = cblock (template "public static $1<$2> jsonBinding()" [jsonBindingI,className])
        (  cblock1 (template "return new $1<$2>()" [jsonBindingI,className])
           (  coverride (template "public $1<$2> factory()" [factoryI,className])
              (  cline "return FACTORY;"
              )
           <> cline ""
           <> coverride (template "public $1 toJson($2 _value)" [jsonElementI,className])
              (  ctemplate "return new $1(_value.toString());" [jsonPrimitiveI]
              )
           <> cline ""
           <> coverride (template "public $1 fromJson($2 _json)" [className,jsonElementI])
              (  cline "try {"
              <> indent (cline "return fromString(_json.getAsString());")
              <> cline "} catch (IllegalArgumentException e) {"
              <> indent (ctemplate "throw new $1(e.getMessage());" [jsonParseExceptionI])
              <> cline "}"
              )
           )
        )

  addMethod (cline "/* Json serialization */")
  addMethod factory

genJsonBindingExpr :: CodeGenProfile -> TypeExpr CResolvedType -> CState T.Text
genJsonBindingExpr cgp (TypeExpr rt params) = do
  bparams <- mapM (genJsonBindingExpr cgp) params
  case rt of
    (RT_Named (scopedName,Decl{d_customType=mct})) -> do
      fscope <- case mct of
        Nothing -> genScopedName scopedName
        (Just ct) -> getHelpers ct
      return (template "$1.jsonBinding($2)" [fscope,commaSep bparams])
    (RT_Param ident) -> return ("binding" <> ident)
    (RT_Primitive pt) -> do
      prim <- primJsonBinding cgp pt
      case bparams of
        [] -> return prim
        _ -> return (template "$1($2)" [prim,commaSep bparams])


primJsonBinding :: CodeGenProfile -> PrimitiveType -> CState T.Text
primJsonBinding cgp pt = do
  jsonBindingsI <- addImport (javaClass (cgp_runtimePackage cgp) "JsonBindings")
  return (jsonBindingsI <> "." <> bindingName pt)
  where
    bindingName P_Void = "VOID"
    bindingName P_Bool = "BOOLEAN"
    bindingName P_Int8 = "BYTE"
    bindingName P_Int16 = "SHORT"
    bindingName P_Int32 = "INTEGER"
    bindingName P_Int64 = "LONG"
    bindingName P_Word8 = "BYTE"
    bindingName P_Word16 = "SHORT"
    bindingName P_Word32 = "INTEGER"
    bindingName P_Word64 = "LONG"
    bindingName P_Float = "FLOAT"
    bindingName P_Double = "DOUBLE"
    bindingName P_Json = "JSON"
    bindingName P_ByteVector = "BYTE_ARRAY"
    bindingName P_String = "STRING"
    bindingName P_Vector = "arrayList"
    bindingName P_StringMap = "stringMap"
    bindingName P_Nullable = "nullable"
