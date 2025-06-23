{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Java.Parcelable(
  generateStructParcelable,
  generateUnionParcelable
  ) where

import Data.Foldable(for_,fold)
import Data.Monoid
import Data.String(IsString(..))
import Data.Traversable(for)

import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Java.Internal
import ADL.Utils.IndentedCode
import ADL.Utils.Format

generateStructParcelable :: CodeGenProfile -> CDecl -> Struct CResolvedType -> [FieldDetails] -> CState ()
generateStructParcelable codeProfile decl struct fieldDetails = do
  let className = unreserveWord (d_name decl)
  idParcel <- addImport "android.os.Parcel"
  idParcelable <- addImport "android.os.Parcelable"
  addImplements idParcelable

  addMethod (cline "/* Android Parcelable implementation */")

  addMethod $ coverride "public int describeContents()" (
    cline "return 0;"
    )

  writeFields <- for fieldDetails $ \fd -> do
    writeToParcel (f_type (fd_field fd)) "out" (fd_memberVarName fd) "flags"

  readFields <- for fieldDetails $ \fd -> do
    readFromParcel (f_type (fd_field fd)) (Just (fd_typeExprStr fd)) (fd_varName fd) "in"

  addMethod $ coverride (template "public void writeToParcel($1 out, int flags)" [idParcel]) (
    mconcat writeFields
    )

  addMethod $ cblock1 (template "public static final $1.Creator<$2> CREATOR = new $1.Creator<$2>()" [idParcelable,className]) (
    coverride (template "public $1 createFromParcel($2 in)" [className,idParcel]) (
      mconcat readFields
      <>
      ctemplate "return new $1($2);" [className,commaSep [fd_varName fd | fd <- fieldDetails]]
      )
    <>
    cline ""
    <>
    coverride (template "public $1[] newArray(int size)" [className]) (
      ctemplate "return new $1[size];" [className]
      )
    )

generateUnionParcelable :: CodeGenProfile -> CDecl -> Union CResolvedType -> [FieldDetails] -> CState ()
generateUnionParcelable codeProfile decl union fieldDetails = do
  let className = unreserveWord (d_name decl)
      discVar = if cgp_hungarianNaming codeProfile then "mDisc" else "disc"
      valueVar = if cgp_hungarianNaming codeProfile then "mValue" else "value"

  idParcel <- addImport "android.os.Parcel"
  idParcelable <- addImport "android.os.Parcelable"
  addImplements "Parcelable"

  addMethod (cline "/* Android Parcelable implementation */")

  addMethod $ coverride "public int describeContents()" (
    cline "return 0;"
    )

  writeFields <- for fieldDetails $ \fd -> do
    writeToParcel (f_type (fd_field fd)) "out" (template "(($1) $2)" [fd_typeExprStr fd,valueVar]) "flags"

  readFields <- for fieldDetails $ \fd -> do
    readFromParcel (f_type (fd_field fd)) Nothing "value" "in"

  addMethod $ coverride (template "public void writeToParcel($1 out, int flags)" [idParcel]) (
    ctemplate "out.writeInt($1.ordinal());" [discVar]
    <>
    cblock (template "switch($1)" [discVar]) (
      mconcat [
        ctemplate "case $1:" [discriminatorName fd]
        <>
        indent (
          writeField
          <>
          cline "break;"
          )
        | (fd,writeField) <- zip fieldDetails writeFields]
      )
    )

  addMethod $ cblock1 (template "public static final $1.Creator<$2> CREATOR = new $1.Creator<$2>()" [idParcelable,className]) (
    coverride (template "public $1 createFromParcel($2 in)" [className,idParcel]) (
      cline "Disc disc = Disc.values()[in.readInt()];"
      <>
      cline "Object value = null;"
      <>
      cblock "switch(disc)" (
        mconcat [
          ctemplate "case $1:" [discriminatorName fd]
          <>
          indent (
            readField
            <>
            cline "break;"
            )
          | (fd,readField) <- zip fieldDetails readFields]
        )
      <>
      ctemplate "return new $1(disc, value);" [className]
      )
    <>
    cline ""
    <>
    coverride (template "public $1[] newArray(int size)" [className]) (
      ctemplate "return new $1[size];" [className]
      )
    )


writeToParcel :: TypeExpr CResolvedType -> Ident -> Ident -> Ident -> CState Code
writeToParcel te to from flags = return $ case te of
  (TypeExpr (RT_Primitive P_Void) _) -> mempty
  (TypeExpr (RT_Primitive P_Bool) _) -> ctemplate "$1.writeByte($2 ? (byte) 1 : (byte) 0);" [to,from]
  (TypeExpr (RT_Primitive P_Int8) _) -> ctemplate "$1.writeByte($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int16) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int32) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int64) _) -> ctemplate "$1.writeLong($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word8) _) -> ctemplate "$1.writeByte($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word16) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word32) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word64) _) -> ctemplate "$1.writeLong($2);" [to,from]
  (TypeExpr (RT_Primitive P_Float) _) -> ctemplate "$1.writeFloat($2);" [to,from]
  (TypeExpr (RT_Primitive P_Double) _) -> ctemplate "$1.writeDouble($2);" [to,from]
  (TypeExpr (RT_Primitive P_ByteVector) _) -> ctemplate "$1.writeByteArray($2.getValue());" [to,from]
  (TypeExpr (RT_Primitive P_String) _) -> ctemplate "$1.writeString($2);" [to,from]
  (TypeExpr (RT_Primitive P_Vector) _) -> ctemplate "$1.writeList($2);" [to,from]
  (TypeExpr (RT_Primitive P_StringMap) _) -> ctemplate "$1.writeStringMap($2);" [to,from]
  (TypeExpr (RT_Primitive P_Nullable) _) -> ctemplate "$1.writeNullable($2);" [to,from]
  _ -> ctemplate "$1.writeToParcel($2, $3);" [from,to,flags]


readFromParcel :: TypeExpr CResolvedType -> Maybe Ident -> Ident -> Ident -> CState Code
readFromParcel te mtotype tovar from = do
  let to = case mtotype of
        Nothing -> tovar
        (Just totype) -> totype <> " " <> tovar
  case te of
    (TypeExpr (RT_Primitive P_Void) _) -> return $ ctemplate "$1 = null;" [to]
    (TypeExpr (RT_Primitive P_Bool) _) -> return $ ctemplate "$1 = $2.readByte() != 0;" [to,from]
    (TypeExpr (RT_Primitive P_Int8) _) -> return $ ctemplate "$1 = $2.readByte();" [to,from]
    (TypeExpr (RT_Primitive P_Int16) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Int32) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Int64) _) -> return $ ctemplate "$1 = $2.readLong();" [to,from]
    (TypeExpr (RT_Primitive P_Word8) _) -> return $ ctemplate "$1 = $2.readByte();" [to,from]
    (TypeExpr (RT_Primitive P_Word16) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Word32) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Word64) _) -> return $ ctemplate "$1 = $2.readLong();" [to,from]
    (TypeExpr (RT_Primitive P_Float) _) -> return $ ctemplate "$1 = $2.readFloat();" [to,from]
    (TypeExpr (RT_Primitive P_Double) _) -> return $ ctemplate "$1 = $2.readDouble();" [to,from]
    (TypeExpr (RT_Primitive P_String) _) -> return $ ctemplate "$1 = $2.readString();" [to,from]
    (TypeExpr (RT_Primitive P_ByteVector) _) -> do
      return (
        ctemplate "$1 = new ByteArray($2.createByteArray());" [to,from]
        )
    (TypeExpr (RT_Primitive P_Vector) [te']) ->  do
      arrayList <- addImport "java.util.ArrayList"
      typeExprStr <- genTypeExprB TypeBoxed te'
      return (
        ctemplate "$1 = new $2<$3>();" [to,arrayList,typeExprStr]
        <>
        ctemplate "$1.readList($2, $3.class.getClassLoader());" [from,tovar,typeExprStr]
        )
    (TypeExpr (RT_Primitive P_StringMap) _) -> return $ ctemplate "$1 = $2.readStringMap();" [to,from]
    (TypeExpr (RT_Primitive P_Nullable) _) -> return $ ctemplate "$1 = $2.readNullable();" [to,from]
    _ -> do
      typeExprStr <- genTypeExprB TypeBoxed te
      return (
        ctemplate "$1 = $2.CREATOR.createFromParcel($3);" [to,typeExprStr,from]
        )
