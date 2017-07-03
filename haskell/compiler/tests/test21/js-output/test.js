// Automatically generated code from adl: test
//
//


const Ann1 = {
  name : "Ann1",
  module : "test",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"Int32"},
  annotations : [
  ]
};

const Ann2 = {
  name : "Ann2",
  module : "test",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"String"},
  annotations : [
  ]
};

const Ann3 = {
  name : "Ann3",
  module : "test",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "f1",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
      ]
    },
    {
      name : "f2",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
      ]
    }
  ],
  annotations : [
  ]
};

const Ann4 = {
  name : "Ann4",
  module : "test",
  kind : "typedef",
  typevars : [],
  type : {"primitive":"Void"},
  annotations : [
  ]
};

const MyStruct = {
  name : "MyStruct",
  module : "test",
  kind : "struct",
  typevars : [[]],
  fields : [
    {
      name : "x",
      type : {"primitive":"Int32"},
      defaultv : null,
      annotations : [
        {
          type : "sys.annotations.Doc",
          value : "The X coordinate\n"
        }
      ]
    },
    {
      name : "y",
      type : {"primitive":"Int32"},
      defaultv : null,
      annotations : [
        {
          type : "sys.annotations.Doc",
          value : "The Y coordinate\n"
        }
      ]
    },
    {
      name : "name",
      type : {"primitive":"String"},
      defaultv : null,
      annotations : [
        {
          type : "test.Ann4",
          value : null
        }
      ]
    }
  ],
  annotations : [
    {
      type : "test.Ann1",
      value : 42
    },
    {
      type : "test.Ann2",
      value : "asdfa"
    }
  ]
};

export const _ADL_TYPES = {
  "test.MyStruct" : MyStruct,
  "test.Ann4" : Ann4,
  "test.Ann3" : Ann3,
  "test.Ann2" : Ann2,
  "test.Ann1" : Ann1
};
