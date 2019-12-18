// Automatically generated code from adl: sys.types
//
//


const Either = {
  name : "Either",
  module : "sys.types",
  kind : "union",
  typevars : [["T1","T2"]],
  fields : [
    {
      name : "left",
      type : {"var":"T1"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "right",
      type : {"var":"T2"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Error = {
  name : "Error",
  module : "sys.types",
  kind : "union",
  typevars : [["T"]],
  fields : [
    {
      name : "value",
      type : {"var":"T"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "error",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Map = {
  name : "Map",
  module : "sys.types",
  kind : "newtype",
  typevars : ["K","V"],
  type : {"args":[{"args":[{"var":"K"},{"var":"V"}],"app":{"ref":"sys.types.Pair"}}],"app":{"primitive":"Vector"}},
  defaultv : null,
  annotations : [
  ]
};

const MapEntry = {
  name : "MapEntry",
  module : "sys.types",
  kind : "struct",
  typevars : [["K","V"]],
  fields : [
    {
      name : "key",
      type : {"var":"K"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "value",
      type : {"var":"V"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Maybe = {
  name : "Maybe",
  module : "sys.types",
  kind : "union",
  typevars : [["T"]],
  fields : [
    {
      name : "nothing",
      type : {"primitive":"Void"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "just",
      type : {"var":"T"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Pair = {
  name : "Pair",
  module : "sys.types",
  kind : "struct",
  typevars : [["T1","T2"]],
  fields : [
    {
      name : "v1",
      type : {"var":"T1"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "v2",
      type : {"var":"T2"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Set = {
  name : "Set",
  module : "sys.types",
  kind : "newtype",
  typevars : ["T"],
  type : {"args":[{"var":"T"}],"app":{"primitive":"Vector"}},
  defaultv : null,
  annotations : [
  ]
};

export const _ADL_TYPES = {
  "sys.types.Set" : Set,
  "sys.types.Pair" : Pair,
  "sys.types.Maybe" : Maybe,
  "sys.types.MapEntry" : MapEntry,
  "sys.types.Map" : Map,
  "sys.types.Error" : Error,
  "sys.types.Either" : Either
};
