// Automatically generated code from adl: sys.sinkimpl
//
//


var SerialisationType = {
  name : "SerialisationType",
  module : "sys.sinkimpl",
  kind : "newtype",
  typevars : [],
  type : {"primitive":"String"},
  defaultv : null,
  annotations : [
  ]
};

var SinkData = {
  name : "SinkData",
  module : "sys.sinkimpl",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "transport",
      type : {"ref":"sys.sinkimpl.TransportName"},
      defaultv : "null",
      annotations : [
      ]
    },
    {
      name : "address",
      type : {"ref":"sys.sinkimpl.TransportAddr"},
      defaultv : {"stringv":""},
      annotations : [
      ]
    },
    {
      name : "serialisation",
      type : {"ref":"sys.sinkimpl.SerialisationType"},
      defaultv : "json",
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var TransportAddr = {
  name : "TransportAddr",
  module : "sys.sinkimpl",
  kind : "union",
  typevars : [[]],
  fields : [
    {
      name : "stringv",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "intv",
      type : {"primitive":"Word64"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "arrayv",
      type : {"args":[{"ref":"sys.sinkimpl.TransportAddr"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var TransportName = {
  name : "TransportName",
  module : "sys.sinkimpl",
  kind : "newtype",
  typevars : [],
  type : {"primitive":"String"},
  defaultv : null,
  annotations : [
  ]
};

function _addTypes(dict) {
  dict["sys.sinkimpl.TransportName"] = TransportName;
  dict["sys.sinkimpl.TransportAddr"] = TransportAddr;
  dict["sys.sinkimpl.SinkData"] = SinkData;
  dict["sys.sinkimpl.SerialisationType"] = SerialisationType;
}

export { _addTypes };
