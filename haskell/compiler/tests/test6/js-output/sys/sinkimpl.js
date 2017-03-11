// Automatically generated code from adl: sys.sinkimpl
//
//


const SerialisationType = {
  name : "SerialisationType",
  module : "sys.sinkimpl",
  kind : "newtype",
  typevars : [],
  type : {"primitive":"String"},
  defaultv : null,
  annotations : [
  ]
};

const SinkData = {
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

const TransportAddr = {
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

const TransportName = {
  name : "TransportName",
  module : "sys.sinkimpl",
  kind : "newtype",
  typevars : [],
  type : {"primitive":"String"},
  defaultv : null,
  annotations : [
  ]
};

export const _ADL_TYPES = {
  "sys.sinkimpl.TransportName" : TransportName,
  "sys.sinkimpl.TransportAddr" : TransportAddr,
  "sys.sinkimpl.SinkData" : SinkData,
  "sys.sinkimpl.SerialisationType" : SerialisationType
};
