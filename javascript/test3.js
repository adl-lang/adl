var core = require("adl/core.js");

function _A() {
  var fieldTypes =  {
    "f_int" : () => core.Int16,
    "f_string" : () => core.String,
    "f_bool" : () => core.Bool
  };
  return core.StructType(fieldTypes,"f_int");
};

var A = _A();

function B(T) {
  var fieldTypes =  {
    "f_t" : () => T,
    "f_string" : () => core.String,
    "f_tvec" : () => core.Vector(T),
    "f_xy" : () => XY(T)
  };
  return core.StructType(fieldTypes,"f_t");
};

function S(T) {
  var fieldTypes =  {
    "f_void" : () => core.Void,
    "f_bool" : () => core.Bool,
    "f_int8" : () => core.Int8,
    "f_int16" : () => core.Int16,
    "f_int32" : () => core.Int32,
    "f_int64" : () => core.Int64,
    "f_word8" : () => core.Word8,
    "f_word16" : () => core.Word16,
    "f_word32" : () => core.Word32,
    "f_word64" : () => core.Word64,
    "f_float" : () => core.Double,
    "f_double" : () => core.Double,
    "f_bytes" : () => core.ByteVector,
    "f_string" : () => core.String,
    "f_vstring" : () => core.Vector(core.String),
    "f_a" : () => A,
    "f_u" : () => U,
    "f_t" : () => T,
    "f_bint16" : () => B(core.Int16),
    "f_smap" : () => core.StringMap(core.Int32)
  };
  return core.StructType(fieldTypes,"f_void");
};

function _U() {
  var fieldTypes =  {
    "f_int" : () => core.Int16,
    "f_string" : () => core.String
  };
  return core.UnionType(fieldTypes,"f_int");
};

var U = _U();

function XY(T) {
  var fieldTypes =  {
    "x" : () => T,
    "y" : () => T
  };
  return core.StructType(fieldTypes,"x");
};

exports.A = A;
exports.B = B;
exports.S = S;
exports.U = U;
exports.XY = XY;
