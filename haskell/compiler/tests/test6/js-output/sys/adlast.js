// Automatically generated code from adl: sys.adlast
//
// depends on adl: sys.types
//


var Annotations = {
  name : "Annotations",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"args":[{"ref":"sys.adlast.ScopedName"},{"ref":"sys.adlast.Literal"}],"app":{"ref":"sys.types.Map"}},
  annotations : [
  ]
};

var Decl = {
  name : "Decl",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "name",
      type : {"ref":"sys.adlast.Ident"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "version",
      type : {"args":[{"primitive":"Word32"}],"app":{"ref":"sys.types.Maybe"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "type_",
      type : {"ref":"sys.adlast.DeclType"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "annotations",
      type : {"ref":"sys.adlast.Annotations"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var DeclType = {
  name : "DeclType",
  module : "sys.adlast",
  kind : "union",
  typevars : [[]],
  fields : [
    {
      name : "struct_",
      type : {"ref":"sys.adlast.Struct"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "union_",
      type : {"ref":"sys.adlast.Union"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "type_",
      type : {"ref":"sys.adlast.TypeDef"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "newtype_",
      type : {"ref":"sys.adlast.NewType"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var DeclVersions = {
  name : "DeclVersions",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"args":[{"ref":"sys.adlast.Decl"}],"app":{"primitive":"Vector"}},
  annotations : [
  ]
};

var Field = {
  name : "Field",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "name",
      type : {"ref":"sys.adlast.Ident"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "typeExpr",
      type : {"ref":"sys.adlast.TypeExpr"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "default",
      type : {"args":[{"ref":"sys.adlast.Literal"}],"app":{"ref":"sys.types.Maybe"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "annotations",
      type : {"ref":"sys.adlast.Annotations"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var Ident = {
  name : "Ident",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"String"},
  annotations : [
  ]
};

var Import = {
  name : "Import",
  module : "sys.adlast",
  kind : "union",
  typevars : [[]],
  fields : [
    {
      name : "moduleName",
      type : {"ref":"sys.adlast.ModuleName"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "scopedName",
      type : {"ref":"sys.adlast.ScopedName"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var Literal = {
  name : "Literal",
  module : "sys.adlast",
  kind : "union",
  typevars : [[]],
  fields : [
    {
      name : "null",
      type : {"primitive":"Void"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "integer",
      type : {"primitive":"Int64"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "double",
      type : {"primitive":"Double"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "string",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "boolean",
      type : {"primitive":"Bool"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "array",
      type : {"args":[{"ref":"sys.adlast.Literal"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "object",
      type : {"args":[{"primitive":"String"},{"ref":"sys.adlast.Literal"}],"app":{"ref":"sys.types.Map"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var Module = {
  name : "Module",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "name",
      type : {"ref":"sys.adlast.ModuleName"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "imports",
      type : {"args":[{"ref":"sys.adlast.Import"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "decls",
      type : {"args":[{"ref":"sys.adlast.Ident"},{"ref":"sys.adlast.Decl"}],"app":{"ref":"sys.types.Map"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var ModuleName = {
  name : "ModuleName",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"String"},
  annotations : [
  ]
};

var NewType = {
  name : "NewType",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeParams",
      type : {"args":[{"ref":"sys.adlast.Ident"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "typeExpr",
      type : {"ref":"sys.adlast.TypeExpr"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "default",
      type : {"args":[{"ref":"sys.adlast.Literal"}],"app":{"ref":"sys.types.Maybe"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var ScopedName = {
  name : "ScopedName",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "moduleName",
      type : {"ref":"sys.adlast.ModuleName"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "name",
      type : {"ref":"sys.adlast.Ident"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var Struct = {
  name : "Struct",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeParams",
      type : {"args":[{"ref":"sys.adlast.Ident"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "fields",
      type : {"args":[{"ref":"sys.adlast.Field"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var TypeDef = {
  name : "TypeDef",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeParams",
      type : {"args":[{"ref":"sys.adlast.Ident"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "typeExpr",
      type : {"ref":"sys.adlast.TypeExpr"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var TypeExpr = {
  name : "TypeExpr",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeRef",
      type : {"ref":"sys.adlast.TypeRef"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "parameters",
      type : {"args":[{"ref":"sys.adlast.TypeExpr"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var TypeRef = {
  name : "TypeRef",
  module : "sys.adlast",
  kind : "union",
  typevars : [[]],
  fields : [
    {
      name : "primitive",
      type : {"ref":"sys.adlast.Ident"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "typeParam",
      type : {"ref":"sys.adlast.Ident"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "reference",
      type : {"ref":"sys.adlast.ScopedName"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

var Union = {
  name : "Union",
  module : "sys.adlast",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeParams",
      type : {"args":[{"ref":"sys.adlast.Ident"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "fields",
      type : {"args":[{"ref":"sys.adlast.Field"}],"app":{"primitive":"Vector"}},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

function _addTypes(dict) {
  dict["sys.adlast.Union"] = Union;
  dict["sys.adlast.TypeRef"] = TypeRef;
  dict["sys.adlast.TypeExpr"] = TypeExpr;
  dict["sys.adlast.TypeDef"] = TypeDef;
  dict["sys.adlast.Struct"] = Struct;
  dict["sys.adlast.ScopedName"] = ScopedName;
  dict["sys.adlast.NewType"] = NewType;
  dict["sys.adlast.ModuleName"] = ModuleName;
  dict["sys.adlast.Module"] = Module;
  dict["sys.adlast.Literal"] = Literal;
  dict["sys.adlast.Import"] = Import;
  dict["sys.adlast.Ident"] = Ident;
  dict["sys.adlast.Field"] = Field;
  dict["sys.adlast.DeclVersions"] = DeclVersions;
  dict["sys.adlast.DeclType"] = DeclType;
  dict["sys.adlast.Decl"] = Decl;
  dict["sys.adlast.Annotations"] = Annotations;
}

export { _addTypes };
