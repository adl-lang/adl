// Automatically generated code from adl: sys.rpc
//
//


var Rpc = {
  name : "Rpc",
  module : "sys.rpc",
  kind : "struct",
  typevars : [["I","O"]],
  fields : [
    {
      name : "params",
      type : {"var":"I"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "replyTo",
      type : {"args":[{"var":"O"}],"app":{"primitive":"Sink"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var RpcSvc = {
  name : "RpcSvc",
  module : "sys.rpc",
  kind : "typedef",
  typevars : ["I","O"],
  type : {"args":[{"args":[{"var":"I"},{"var":"O"}],"app":{"ref":"sys.rpc.Rpc"}}],"app":{"primitive":"Sink"}},
  annotations : [
  ]
};

function _addTypes(dict) {
  dict["sys.rpc.RpcSvc"] = RpcSvc;
  dict["sys.rpc.Rpc"] = Rpc;
}

export { _addTypes };
