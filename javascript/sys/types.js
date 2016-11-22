var core = require("adl/core.js");

function Either(T1,T2) {
  var fields =  [
    ["left", () => T1],
    ["right", () => T2]
  ];
  return core.UnionType("Either",fields);
};

function Error(T) {
  var fields =  [
    ["value", () => T],
    ["error", () => core.String]
  ];
  return core.UnionType("Error",fields);
};

function Map(K,V) {
  return core.Vector(Pair(K, V));
};

exports.Map = Map;

function Maybe(T) {
  var fields =  [
    ["nothing", () => core.Void],
    ["just", () => T]
  ];
  return core.UnionType("Maybe",fields);
};

function Nullable(T) {
  var fields =  [
    ["nothing", () => core.Void],
    ["just", () => T]
  ];
  return core.UnionType("Nullable",fields);
};

function Pair(T1,T2) {
  var fields =  [
    ["v1", () => T1],
    ["v2", () => T2]
  ];
  return core.StructType("Pair",fields);
};

function Set(T) {
  return core.Vector(T);
};

exports.Set = Set;

exports.Either = Either;
exports.Error = Error;
exports.Maybe = Maybe;
exports.Nullable = Nullable;
exports.Pair = Pair;
