// Automatically generated code from adl: sys.dynamic
//
// depends on adl: sys.adlast
//


const Dynamic = {
  name : "Dynamic",
  module : "sys.dynamic",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "typeExpr",
      type : {"ref":"sys.adlast.TypeExpr"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "value",
      type : {"primitive":"Json"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
    {
      type : "sys.annotations.Doc",
      value : "A serialised value along with  its type\n"
    }
  ]
};

export const _ADL_TYPES = {
  "sys.dynamic.Dynamic" : Dynamic
};
