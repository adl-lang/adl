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

(docs to be completed)

# Custom types

(docs to be completed)

## Standard Custom Types

(docs to be completed)
