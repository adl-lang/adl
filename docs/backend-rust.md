# ADL Rust backend

```
Usage: adlc rust [OPTION...] files...
  -I DIR  --searchdir=DIR        Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR        Set the directory where generated code is written
          --merge-adlext=EXT     Add the specifed adl file extension to merged on loading
          --verbose              Print extra diagnostic information, especially about files being read/written
          --no-overwrite         Don't update files that haven't changed
          --manifest=FILE        Write a manifest file recording generated files
          --generate-transitive  Also generate code for the transitive dependencies of the specified adl files
          --package=PACKAGE      The language package into which the generated ADL code will be placed
  -R DIR  --runtime-module=DIR   Set the module where the runtime is located
```

# Generated Code

The backend generates rust code from the input ADL
files. Each ADL module results in a single rust module. The generated
rust modules will reflect the structure of the ADL source modules, but
be placed beneath the rust module specified with the `--package` option.
Appropriate `mod.rs` files will also be generated.

For example, the ADL modules:

```
a.adl
b/x.adl
b/y.adl
```

when compiled with `--package myproject::adl` will result in the following
rust files

```
myproject/adl/mod.rs
myproject/adl/a.rs
myproject/adl/b/mod.rs
myproject/adl/b/x.rs
myproject/adl/b/y.rs
```

It is a requirement that the argument to `--package` is relative
to the root of the crate being built.

## ADL Primitives

The ADL primitive types are mapped to rust types as follows:

| ADL Type                     | Rust Type                   |
|------------------------------|--------------------------------|
| `Int8,Int16,Int32,Int64`     | `i8,i16,i32,i64`|
| `Word8,Word16,Word32,Word64` | `u8,u16,u32,u64` |
| `Bool`                       | `bool`                         |
| `Void`                       | `()`                         |
| `Float,Double`               | `f32,f64`                 |
| `String`                     | `String`                  |
| `Bytes`                      | `adlrt::ByteVector`       |
| `Vector<T>`                  | `Vec<T>`               |
| `StringMap<T>`               | `std::collections::HashMap<String,T>`      |
| `Nullable<T>`                | `Option<T>`                |

## ADL structs

Struct declarations in ADL:

```
    struct Rectangle
    {
        Double width;
        Double height;
    };
    
```

generate rust structs in the expected way:


```
#[derive(Serialize,Deserialize)]
pub struct Rectangle {
  pub width: f64,

  pub height: f64,
}

impl Rectangle {
  pub fn new(width: f64, height: f64) -> Rectangle {
    Rectangle {
      width: width,
      height: height,
    }
  }
}
```

## ADL unions

ADL unions:

```
    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        Translated<Picture> translated;
    };
```

map to rust enums:

```
#[derive(Serialize,Deserialize)]
pub enum Picture {
  #[serde(rename="circle")]
  Circle(Circle),

  #[serde(rename="rectangle")]
  Rectangle(Rectangle),

  #[serde(rename="composed")]
  Composed(Vec<Picture>),

  #[serde(rename="translated")]
  Translated(Box<Translated<Picture>>),
}

```

## ADL newtypes

ADL newtypes
```
    newtype Factory = String;
```

map to rust structs with a single unamed element:

```
#[derive(Serialize,Deserialize)]
pub struct Factory(pub String);
```

## ADL type aliases

ADL type aliases

```
type Int1 = Int64;
```

map directly to their corresponding rust type aliases

```
pub type Int1 = i64;
```

## Generic types

The examples above are all for monorphic ADL declarations. Polymorphic ADL declarations
map to rust declarations as would be expected.

# Runtime

(docs to be completed)

# Annotations

The rust backend reads the annotations declared in
[`adlc.config.rust`](../adl/stdlib/adlc/config/rust.adl). Any other annotation is
ignored, unless it is marked for emission as described under
`RustEmitAnnotation` below.

| Annotation | Applies to | Effect |
| --- | --- | --- |
| `RustGenerate` | module, declaration | Whether code is generated at all |
| `RustCustomType` | declaration | Use a hand written type in place of the generated one |
| `RustStorageModel` | field | Store the field inline (default) or in a `Box<>` |
| `RustExtraDeriveTraits` | declaration | Add names to the generated `#[derive(..)]` |
| `RustEmitAnnotation` | an annotation's declaration | Carry that annotation through into the generated code |

## RustExtraDeriveTraits

Adds names to a declaration's derive attribute. They are emitted verbatim, so
they may be qualified paths:

```
annotation MyStruct RustExtraDeriveTraits ["Debug", "my_crate::MyDerive"];
```

Traits already being derived are ignored, as are `Serialize` and `Deserialize`
on a newtype, where those impls are hand written and a derive would collide
with them. A type alias has no derive attribute, so the annotation has no
effect there.

## RustEmitAnnotation

By default the rust backend discards annotations it does not itself understand.
`RustEmitAnnotation` marks an annotation so that its uses are carried through
into the generated code, where a proc macro can read them at compile time.

It is applied once, to the declaration of the annotation itself:

```
// in my/module.adl-rs
import adlc.config.rust.*;

annotation MyPath RustEmitAnnotation null;
```

The explicit `null` is needed in the side file form, which has no syntax for a
`Void` value; inline on the declaration it is just `@RustEmitAnnotation`.

Thereafter every use of `MyPath` is emitted beside the generated item:

```
@MyPath "/healthy"
HttpReq<Void,Void> healthy = { "security": "public" };
```

```rust
#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize,adl_rt::AdlAnnotations)]
pub struct ApiRequests {
  #[adl_annotation(name="common.http.MyPath", value="\"/healthy\"")]
  pub healthy: HttpReq<(), ()>,
```

`value` is the annotation's value in ADL's json encoding, passed through
uninterpreted, so this works for an annotation of any type.

Note the extra derive. Rust rejects an attribute that no derive on the item has
declared as a helper, so `adl_rt::AdlAnnotations` is added wherever an
annotation is emitted. **Generated code using emitted annotations therefore
depends on the `adl_rt` crate**, which re-exports the derive from
`adl_rt_derive`, whose source is in
[`rust/runtime-derive`](../rust/runtime-derive). One dependency rather than
two, in the same way that `serde::Serialize` is a re-export of
`serde_derive::Serialize`.

That derive generates nothing; it exists only to make the attribute legal and
inert. Other derive macros on the same item can still read it, as rust makes
helper attributes visible to every macro rather than only to the one that
declared them.

`--include-rt`, which copies the runtime in as source rather than depending on
the crate, remains supported and unchanged. A project combining it with
emitted annotations needs `adl_rt` for the derive regardless, and so carries a
second, unused copy of the runtime types. The two are alternatives; depending
on the crate is the direction of travel.

Emission works on struct and union declarations, struct fields, union branches,
and newtype declarations. It does **not** work on a type alias, which has no
derive attribute and so cannot introduce the required helper; annotations on a
type alias are silently not emitted.

# Custom types

(docs to be completed)

## Standard Custom Types

(docs to be completed)
