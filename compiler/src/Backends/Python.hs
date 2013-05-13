module Python where

import Control.Monad
import AST

type Text = String

data PythonType = PythonType {
    pt_isMutable ::  Bool,
    pt_mkDefaultInstance :: String,
    pt_mkInstance :: Literal -> String
}

data PythonGenerator a = PythonGenerator
instance Monad PythonGenerator where
  return = undefined
  (>>=) = undefined

writeLine :: ModuleName -> Text -> PythonGenerator ()
writeLine = undefined

indent, unindent :: ModuleName -> PythonGenerator ()
indent = undefined
unindent = undefined

template :: Text -> [Text] -> Text
template = undefined

intersperse :: Text -> [Text] -> Text
intersperse = undefined

withIndent :: ModuleName -> PythonGenerator a -> PythonGenerator a
withIndent m g = do
    indent m
    a <- g
    unindent m
    return a

getPyType :: ResolvedDecl -> PythonGenerator PythonType
getPyType = undefined

pyFieldName :: Field ResolvedDecl -> Text
pyFieldName = undefined

generate :: Decl ResolvedDecl -> PythonGenerator ()
generate d@(Decl{d_type=(Decl_Struct s)}) = do

    ptypes <- mapM getPyType [f_type f | f <- s_fields s]
    
    let sn = d_name d
        wl = writeLine (sn_moduleName sn)
        wt t ss = writeLine (sn_moduleName sn) (template t ss)
        indent = withIndent (sn_moduleName sn)

        fields = [ (f,pt,pt_mkInstance pt (f_default f)) | (f,pt) <- zip (s_fields s) ptypes]
        fieldTuple oname = intersperse "," [ template "$1.$2" [oname,pyFieldName f] | f <- s_fields s]
                
    wt "class $1:" [sn_name sn]
    indent $ do
        wl "def __init__("
        indent $ do
            forM_ fields $ \(f,pt,defv) -> do
                wt "$1=$2," [pyFieldName f, if pt_isMutable pt then "DEFAULT" else defv]
            wl "):"
            forM_ fields $ \(f,pt,defv) -> do
                if pt_isMutable pt
                  then wt "self.$1 = $1" [pyFieldName f]
                  else wt "self.$1 = $1 if $1 != DEFAULT else $2" [pyFieldName f,defv]
        wl ""
        wl "def __cmp__(self,other):"
        indent $ do
            wl "return cmp("
            indent $ do
                wt "$1," [fieldTuple "self"]
                wt "$1 )" [fieldTuple "other"]
        wl ""
        wl "def __hash__(self,other):"
        indent $ do
            wt "return hash( $1 )" [fieldTuple "self"]

    
        
      
      
