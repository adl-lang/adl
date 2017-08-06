# ADL typescript backend

```
Usage: adlc typescript [OPTION...] files...
  -I DIR  --searchdir=DIR     Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR     Set the directory where generated code is written
          --merge-adlext=EXT  Add the specifed adl file extension to merged on loading
          --verbose           Print extra diagnostic information, especially about files being read/written
          --no-overwrite      Don't update files that haven't changed
          --include-rt        Generate the runtime code
          --exclude-ast       Exclude the generated ASTs
  -R DIR  --runtime-dir=DIR   Set the directory where runtime code is written
```

# Generated Code

The typescript backend generates typescript code from the input ADL
files. Each ADL module results in a typescript module.

ADL structs such as

```
    struct Rectangle
    {
        Double width;
        Double height;
    };
```

produce typescript interfaces (see [Rectangle][ts-rectangle]). In
addition, a helper constructor function
[`makeRectangle`][ts-makerectangle] is also generated. Fields of the
ADL struct that have default values specified are optional parameters
to the constructor function.

ADL unions such as

```
    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        Translated<Picture> translated;
    };
```

produce typescript discriminated unions ([Picture][ts-picture]). Refer
to the [typescript docs][ts-advancedtypes] for more information on the
discriminated union pattern in typescript.

ADL newtypes and type aliases are eliminated in the generated
typescript code by substitution.

Unlike the the haskell and java language backends, the typescript
backend doesn't generate code for serialization. Instead it generates
a representation of each ADL type, available at runtime. This
representation can be processed generically to implement serialization
and potentially many other capabilities. For each ADL type T, the
typescript backend will also generate:

* an ast representation (as a value of type [ScopedDecl][ts-scopeddecl])
* a type expression function (returning a value of type [ATypeExpr<T>][ts-atypeexpr])

Together with a standard [`DeclResolver`][ts-declresolver] interface,
these are sufficient to provide runtime access to ADL type information.

# Serialization

The [json module][ts-json] makes use of the runtime ast to provide
this API:

```
type Json = {}|null;

export interface JsonBinding<T> {
  toJson (t : T): Json;
  fromJson(json : Json) : T;
};

function createJsonBinding<T>(dresolver : DeclResolver, texpr : ATypeExpr<T>) : JsonBinding<T> {
...
}
```

Hence, a concrete example to serialize a value:

```
const pictureJB : JsonBinding<Picture> = createJsonBinding(dresolver, picturemodule.texprPicture());
const p : Picture = ...;
const seralized = JSON.stringify(pictureJB.toJson(p));
```

This approach works for arbitrary ADL Types, including generics.

[ts-rectangle]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L52
[ts-makerectangle]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L57
[ts-picture]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L22
[ts-advancedtypes]:https://www.typescriptlang.org/docs/handbook/advanced-types.html
[ts-scopeddecl]:../adl/stdlib/sys/adlast.adl#L93
[ts-atypeexpr]:../haskell/compiler/lib/typescript/runtime/adl.ts#5
[ts-declresolver]:../haskell/compiler/lib/typescript/runtime/adl.ts#10
[ts-json]:../haskell/compiler/lib/typescript/runtime/json.ts
