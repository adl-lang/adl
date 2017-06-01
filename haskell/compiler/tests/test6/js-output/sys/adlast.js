// Automatically generated code from adl: sys.adlast
//
// depends on adl: sys.types
//


const Annotations = {
  name : "Annotations",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"args":[{"ref":"sys.adlast.ScopedName"},{"ref":"sys.adlast.Literal"}],"app":{"ref":"sys.types.Map"}},
  annotations : [
  ]
};

const Decl = {
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

const DeclType = {
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

const DeclVersions = {
  name : "DeclVersions",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"args":[{"ref":"sys.adlast.Decl"}],"app":{"primitive":"Vector"}},
  annotations : [
  ]
};

const Field = {
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
      name : "serializedName",
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

const Ident = {
  name : "Ident",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"String"},
  annotations : [
  ]
};

const Import = {
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

const Literal = {
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

const Module = {
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

const ModuleName = {
  name : "ModuleName",
  module : "sys.adlast",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"String"},
  annotations : [
  ]
};

const NewType = {
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

const ScopedDecl = {
  name : "ScopedDecl",
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
      name : "decl",
      type : {"ref":"sys.adlast.Decl"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const ScopedName = {
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

const Struct = {
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

const TypeDef = {
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

const TypeExpr = {
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

const TypeRef = {
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

const Union = {
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

export const _ADL_TYPES = {
  "sys.adlast.Union" : Union,
  "sys.adlast.TypeRef" : TypeRef,
  "sys.adlast.TypeExpr" : TypeExpr,
  "sys.adlast.TypeDef" : TypeDef,
  "sys.adlast.Struct" : Struct,
  "sys.adlast.ScopedName" : ScopedName,
  "sys.adlast.ScopedDecl" : ScopedDecl,
  "sys.adlast.NewType" : NewType,
  "sys.adlast.ModuleName" : ModuleName,
  "sys.adlast.Module" : Module,
  "sys.adlast.Literal" : Literal,
  "sys.adlast.Import" : Import,
  "sys.adlast.Ident" : Ident,
  "sys.adlast.Field" : Field,
  "sys.adlast.DeclVersions" : DeclVersions,
  "sys.adlast.DeclType" : DeclType,
  "sys.adlast.Decl" : Decl,
  "sys.adlast.Annotations" : Annotations
};
