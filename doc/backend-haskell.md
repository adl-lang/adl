# ADL haskell backend

```
Usage: adlc haskell [OPTION...] files...
  -I DIR  --searchdir=DIR      Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR      Set the directory where generated code is written
          --merge-adlext=EXT   Add the specifed adl file extension to merged on loading
          --verbose            Print extra diagnostic information, especially about files being read/written
          --no-overwrite       Don't update files that haven't changed
          --package=PACKAGE    The language package into which the generated ADL code will be placed
          --include-rt         Generate the runtime code
          --rtpackage=PACKAGE  The haskell package where the ADL runtime is located
```

# Generated Code

The haskell backend generates haskell code from the input ADL
files. Each ADL module results in a single corresponding haskell
module. The `--package` compiler flag specifies the root package for
generate code. Hence, an adl module `foo.bar` with the compile flag
`--package project1.adl` would result in the haskell module
`project1.adl.foo.bar`.

ADL structs and unions:

```
    struct Rectangle
    {
        Double width;
        Double height;
    };

    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        Translated<Picture> translated;
    };
```

produce haskell data definitions:

```
data Rectangle = Rectangle
    { rectangle_width :: Prelude.Double
    , rectangle_height :: Prelude.Double
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

data Picture
    = Picture_circle Circle
    | Picture_rectangle Rectangle
    | Picture_composed [Picture]
    | Picture_translated (Translated Picture)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)
```

Note that the constructors and field declarations are prefixed with
the type name, to avoid conflicts with other declarations in the same
module. The standard typeclasses `Eq`, `Ord` and `Show` are derived
when possible.

ADL newtypes and type alias produce corresponding haskell definitions
in the obvious way.

Every generated haskell type is also an instance of the
[AdlValue][adlvalue] typeclass, primarily to support json parsing:

```
class AdlValue a where
  -- | A text string describing the type.
  atype :: Proxy a -> T.Text

  -- | A JSON generator for this ADL type
  jsonGen :: JsonGen a

  -- | A JSON parser for this ADL type
  jsonParser :: JsonParser a
```

[adlvalue]:../haskell/runtime/src/ADL/Core/Value.hs#L63

# Primitive Types

The ADL primitive types are mapped to haskell types as follows:

| ADL Type                     | Haskell Type                 |
|------------------------------|------------------------------|
| `Int8,Int16,Int32,Int64`     | `Int8,Int16,Int32,Int64`     |
| `Word8,Word16,Word32,Word64` | `Word8,Word16,Word32,Word64` |
| `Bool`                       | `Bool`                       |
| `Void`                       | `()`                         |
| `Float,Double`               | `Float,Double`               |
| `String`                     | `Data.Text`                  |
| `ByteVector`                 | `Data.ByteString`            |
| `Vector<T>`                  | `[t]`                        |
| `StringMap<T>`               | `Data.Map Data.Text t`       |
| `Nullable<T>`                | `Maybe t`                    |

# Runtime

The generated code depends upon a small runtime. The location of the
runtime in the haskell module hierarchy can be controlled with the
`--rtpackage` compiler flag. As a convenience, when the `--include-rt`
flag is specified, the adl compiler will also output the runtime code.

As a concrete example, if the adl compiler is called like this:

```
adlc haskell\
  --outputdir src \
  --package adl \
  --rtpackage adl/runtime \
  --include-rt \
  picture.adl
```
The following files will be created:

```
./src/adl/Picture.hs
./src/adl/runtime/Value.hs
./src/adl/runtime.hs
```

The runtime itself depends on the following haskell packages:

* aeson
* base
* base64-bytestring
* bytestring
* containers
* scientific
* text
* unordered-containers
* vector

ie the transitive dependencies of the aeson package.

# Annotations

The haskell backend merges annotations from files with an `.adl-hs`
suffix: eg when loading `demo/model.adl` it will automatically merge
`demo/model.adl-hs` if found.

# Custom types

When a type is defined in ADL, a (language independent) serialisation
specification is implied. Running the compiler with a given language
target will generate definitions for that type, as well the necessary
serialisation code.  However, often one would like to use an
equivalent existing type, for compatibility with other code. Custom
types make this possible.

As an example, consider a date type. There is no primitive in the ADL
language for dates, so we need to define one. A possible ADL definition is:

```
newtype Date = String;   // dates are serialised as ISO-8601 strings.
```

This says that a Date is a new independent type, isomorphic to a
string. This would normally result in the following generated haskell
code:

```Haskell
newtype Date = Date { unDate :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Date where ...
```

The issue here is that all user written code making use of ADL values
must be prepared to convert from date strings to more appropriate
native types. A solution is to use a custom type mapping to map the
ADL Date definition to the [Day][dayhs] type from the standard
library. This means that, in all generated haskell code, Day will be
used in lieu of the Date declaration shown above. The developer no
longer need to write any conversion code - all dates throughout the
ADL system use the library type of her choice.

A key thing to realise here is that, even when custom type mappings
are being used, the ADL definitions fix the serialisation of the data
types. This is what permits interoperability between processes coded
in different languages.

Custom types are specified through ADL annotations. To integrate an
arbitrary type into ADL, once needs to provide:

  * an `AdlValue` instance declaration
  * a constructor function (or functions, for a union)

A suitable [haskell declaration][datehs] for such a Date type can be
found in the unit tests.

Then, an annotation tells the ADL compiler to subsitute this custom
definition in place of the automatically generated one:

```
newtype Date = String;   // dates are serialised as ISO-8601 strings.

annotation Date HaskellCustomType {
    "haskellname" : "Date",
    "haskellimports" : [
        "qualified Date"
     ],
    "insertCode" : [
        "type Date = Date.Date"
    ],
    "structConstructor" : "Date.fromText"
};
```

## Standard Custom Types

There are predefined haskell custom type mappings for the following
declarations in the [adl standard library][stdlib]:

| ADL Type               | Haskell Type   |
|------------------------|----------------|
| sys.types.Pair<T1,T2>  | (,)            |
| sys.types.Map<K,V>     | Data.Map.Map   |
| sys.types.Set<V>       | Data.Set.Set   |
| sys.types.Maybe<T>     | prelude.Maybe  |
| sys.types.Either<T1,T> | prelude.Either |


[dayhs]: http://hackage.haskell.org/package/time-1.1.2.1/docs/Data-Time-Calendar.html#t%3ADay
[datehs]:../haskell/compiler/tests/test4/input/Date.hs
[stdlib]:../adl/stdlib
