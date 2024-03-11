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

const Map = {
  name : "Map",
  module : "sys.types",
  kind : "newtype",
  typevars : ["K","V"],
  type : {"app":{"primitive":"Vector"},"args":[{"app":{"ref":"sys.types.MapEntry"},"args":[{"var":"K"},{"var":"V"}]}]},
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

const Result = {
  name : "Result",
  module : "sys.types",
  kind : "union",
  typevars : [["T","E"]],
  fields : [
    {
      name : "ok",
      type : {"var":"T"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "error",
      type : {"var":"E"},
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
  type : {"app":{"primitive":"Vector"},"args":[{"var":"T"}]},
  defaultv : null,
  annotations : [
  ]
};

export const _ADL_TYPES = {
  "sys.types.Set" : Set,
  "sys.types.Result" : Result,
  "sys.types.Pair" : Pair,
  "sys.types.Maybe" : Maybe,
  "sys.types.MapEntry" : MapEntry,
  "sys.types.Map" : Map,
  "sys.types.Either" : Either
};
