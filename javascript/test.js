var core = require("adl/core.js");

function _X1() {
  var fields =  [
    ["f1", () => core.Double],
    ["f2", () => Y1]
  ];
  return core.UnionType("X1",fields);
};

var X1 = _X1();

function _X2() {
  var fields =  [
    ["f1", () => core.Double],
    ["f2", () => core.Vector(Y2)]
  ];
  return core.StructType("X2",fields);
};

var X2 = _X2();

function _Y1() {
  var fields =  [
    ["f1", () => core.String],
    ["f2", () => X1]
  ];
  return core.UnionType("Y1",fields);
};

var Y1 = _Y1();

function _Y2() {
  var fields =  [
    ["f1", () => core.String],
    ["f2", () => core.Vector(X2)]
  ];
  return core.StructType("Y2",fields);
};

var Y2 = _Y2();

exports.X1 = X1;
exports.X2 = X2;
exports.Y1 = Y1;
exports.Y2 = Y2;
