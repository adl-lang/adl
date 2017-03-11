// Automatically generated code from adl: sys.rpc
//
//


const Rpc = {
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

const RpcSvc = {
  name : "RpcSvc",
  module : "sys.rpc",
  kind : "typedef",
  typevars : ["I","O"],
  type : {"args":[{"args":[{"var":"I"},{"var":"O"}],"app":{"ref":"sys.rpc.Rpc"}}],"app":{"primitive":"Sink"}},
  annotations : [
  ]
};

export const _ADL_TYPES = {
  "sys.rpc.RpcSvc" : RpcSvc,
  "sys.rpc.Rpc" : Rpc
};
