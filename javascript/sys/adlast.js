var core = require("adl/core.js");
var types = require("sys/types.js");

exports.Annotations = types.Map(ScopedName, Literal);

function _Decl() {
  var fields =  [
    ["name", () => Ident],
    ["version", () => types.Maybe(core.Word32)],
    ["type_", () => DeclType],
    ["annotations", () => Annotations]
  ];
  return core.StructType("Decl",fields);
};

var Decl = _Decl();

function _DeclType() {
  var fields =  [
    ["struct_", () => Struct],
    ["union_", () => Union],
    ["type_", () => TypeDef],
    ["newtype_", () => NewType]
  ];
  return core.UnionType("DeclType",fields);
};

var DeclType = _DeclType();

exports.DeclVersions = core.Vector(Decl);

function _Field() {
  var fields =  [
    ["name", () => Ident],
    ["typeExpr", () => TypeExpr],
    ["default", () => types.Maybe(Literal)],
    ["annotations", () => Annotations]
  ];
  return core.StructType("Field",fields);
};

var Field = _Field();

exports.Ident = core.String;

function _Import() {
  var fields =  [
    ["moduleName", () => ModuleName],
    ["scopedName", () => ScopedName]
  ];
  return core.UnionType("Import",fields);
};

var Import = _Import();

function _Literal() {
  var fields =  [
    ["null", () => core.Void],
    ["integer", () => core.Int64],
    ["double", () => core.Double],
    ["string", () => core.String],
    ["boolean", () => core.Bool],
    ["array", () => core.Vector(Literal)],
    ["object", () => types.Map(core.String, Literal)]
  ];
  return core.UnionType("Literal",fields);
};

var Literal = _Literal();

function _Module() {
  var fields =  [
    ["name", () => ModuleName],
    ["imports", () => core.Vector(Import)],
    ["decls", () => types.Map(Ident, Decl)]
  ];
  return core.StructType("Module",fields);
};

var Module = _Module();

exports.ModuleName = core.String;

function _NewType() {
  var fields =  [
    ["typeParams", () => core.Vector(Ident)],
    ["typeExpr", () => TypeExpr],
    ["default", () => types.Maybe(Literal)]
  ];
  return core.StructType("NewType",fields);
};

var NewType = _NewType();

function _ScopedName() {
  var fields =  [
    ["moduleName", () => ModuleName],
    ["name", () => Ident]
  ];
  return core.StructType("ScopedName",fields);
};

var ScopedName = _ScopedName();

function _Struct() {
  var fields =  [
    ["typeParams", () => core.Vector(Ident)],
    ["fields", () => core.Vector(Field)]
  ];
  return core.StructType("Struct",fields);
};

var Struct = _Struct();

function _TypeDef() {
  var fields =  [
    ["typeParams", () => core.Vector(Ident)],
    ["typeExpr", () => TypeExpr]
  ];
  return core.StructType("TypeDef",fields);
};

var TypeDef = _TypeDef();

function _TypeExpr() {
  var fields =  [
    ["typeRef", () => TypeRef],
    ["parameters", () => core.Vector(TypeExpr)]
  ];
  return core.StructType("TypeExpr",fields);
};

var TypeExpr = _TypeExpr();

function _TypeRef() {
  var fields =  [
    ["primitive", () => Ident],
    ["typeParam", () => Ident],
    ["reference", () => ScopedName]
  ];
  return core.UnionType("TypeRef",fields);
};

var TypeRef = _TypeRef();

function _Union() {
  var fields =  [
    ["typeParams", () => core.Vector(Ident)],
    ["fields", () => core.Vector(Field)]
  ];
  return core.StructType("Union",fields);
};

var Union = _Union();

exports.Decl = Decl;
exports.DeclType = DeclType;
exports.Field = Field;
exports.Import = Import;
exports.Literal = Literal;
exports.Module = Module;
exports.NewType = NewType;
exports.ScopedName = ScopedName;
exports.Struct = Struct;
exports.TypeDef = TypeDef;
exports.TypeExpr = TypeExpr;
exports.TypeRef = TypeRef;
exports.Union = Union;
