# ADL typescript backend

```
Usage: adlc typescript [OPTION...] files...
  -I DIR  --searchdir=DIR                         Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR                         Set the directory where generated code is written
          --merge-adlext=EXT                      Add the specifed adl file extension to merged on loading
          --verbose                               Print extra diagnostic information, especially about files being read/written
          --no-overwrite                          Don't update files that haven't changed
          --manifest=FILE                         Write a manifest file recording generated files
          --generate-transitive                   Also generate code for the transitive dependencies of the specified adl files
          --include-rt                            Generate the runtime code
          --ts-style=tsc|deno                     Select the style of typescript to be generated
          --include-resolver                      Generate the resolver map for all generated adl files
          --exclude-ast                           Exclude the generated ASTs
          --excluded-ast-annotations=SCOPEDNAMES  Set the annotations to be excluded from the ast (comma separated, default=sys.annotations.Doc)
  -R DIR  --runtime-dir=DIR                       Set the directory where runtime code is written
```

The typescript runtime and ADL generated code depend on ES6 (so use `--target ES6` for the tsc compiler).

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
these are sufficient to provide runtime access to ADL type
information. In order to build a `DeclResolver`, each generated ADL
modules contains an `_AST_MAP` value, which can be passed to
a runtime provided [helper function][ts-declresolverhelper]. One
generally has a single global `DeclResolver` instance for an
application, that can resolve all available ADL definitions. For
example:

```
import * as adl    from "./adlgen/runtime/adl";
import * as common from './adlgen/common';
import * as ui     from './adlgen/common/ui';

export const DECL_RESOLVER = adl.declResolver({
  ...common._AST_MAP,
  ...ui._AST_MAP
});
```

# Runtime

A small amount of runtime code is required to support the ADL generated typescript. This code is referenced as `@adllang/adl-runtime` and  published to both [JSR][adl-runtime-jsr] and [NPM][adl-runtime-npm].

Alternatively, the adl compiler can embed the runtime code in the generated typescript via the `--include-rt` and 
`--runtime-dir` flags.

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

# Annotations

The typescript backend merges annotations from files with an `.adl-ts`
suffix: eg when loading `demo/model.adl` it will automatically merge
`demo/model.adl-ts` if found.

Any `Doc` annotations (which can also be specified using `///`
comments), are included as comments in the generated typescript code.

The [`TypescriptGenerate`][typescript-annotations] annotation can be applied to
modules or declarations to disable code generation. This is useful if
you have a large tree of ADL only some of which needs generated typescript
code.

[ts-rectangle]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L65
[ts-makerectangle]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L70
[ts-picture]:../haskell/compiler/tests/demo1/ts-output/picture.ts#L22
[ts-advancedtypes]:https://www.typescriptlang.org/docs/handbook/advanced-types.html
[ts-scopeddecl]:../adl/stdlib/sys/adlast.adl#L93
[ts-atypeexpr]:../typescript/runtime/adl.ts#5
[ts-declresolver]:../typescript/runtime/adl.ts#10
[ts-declresolverhelper]:../typescript/runtime/adl.ts#14
[ts-json]:../typescript/runtime/json.ts
[typescript-annotations]:../haskell/compiler/lib/adl/adlc/config/typescript.adl
[adl-runtime-jsr]: https://jsr.io/@adllang/adl-runtime
[adl-runtime-npm]: https://www.npmjs.com/package/@adllang/adl-runtime
