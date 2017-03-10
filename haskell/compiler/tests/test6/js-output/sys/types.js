// Automatically generated code from adl: sys.types
//
//


var Either = {
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

var Error = {
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

var Map = {
  name : "Map",
  module : "sys.types",
  kind : "newtype",
  typevars : ["K","V"],
  type : {"args":[{"args":[{"var":"K"},{"var":"V"}],"app":{"ref":"sys.types.Pair"}}],"app":{"primitive":"Vector"}},
  defaultv : null,
  annotations : [
  ]
};

var Maybe = {
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

var Nullable = {
  name : "Nullable",
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
    {
      type : "sys.annotations.CustomSerialization",
      value : true,
    },
    {
      type : "sys.annotations.Doc",
      value : "Nullable<T> is isomorphic to Maybe<T> but with an alternative\njson serialisation, where the null_ branch is represented by a\njson null Value.\n",
    }
  ]
};

var Pair = {
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

var Set = {
  name : "Set",
  module : "sys.types",
  kind : "newtype",
  typevars : ["T"],
  type : {"args":[{"var":"T"}],"app":{"primitive":"Vector"}},
  defaultv : null,
  annotations : [
  ]
};

function _addTypes(dict) {
  dict["sys.types.Set"] = Set;
  dict["sys.types.Pair"] = Pair;
  dict["sys.types.Nullable"] = Nullable;
  dict["sys.types.Maybe"] = Maybe;
  dict["sys.types.Map"] = Map;
  dict["sys.types.Error"] = Error;
  dict["sys.types.Either"] = Either;
}

export { _addTypes };
