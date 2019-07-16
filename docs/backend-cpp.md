# ADL C++ backend

```
Usage: adlc cpp [OPTION...] files...
  -I DIR  --searchdir=DIR       Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR       Set the directory where generated code is written
          --merge-adlext=EXT    Add the specifed adl file extension to merged on loading
          --verbose             Print extra diagnostic information, especially about files being read/written
          --no-overwrite        Don't update files that haven't changed
          --include-prefix=DIR  The prefix to be used to generate/reference include files
          --exclude-relops      Exclude generated code for relational operators
```

# Generated Code

The backend generates c++ code from the input ADL
files. Each ADL module results in a c++ header and implementation
file.

## ADL Primitive

The ADL primitive types are mapped to c++ types as follows:

| ADL Type                     | Haskell Type                   |
|------------------------------|--------------------------------|
| `Int8,Int16,Int32,Int64`     | `int8_t,int16_t,int32_t,int64_t`|
| `Word8,Word16,Word32,Word64` | `uint8_t,uint16_t,uint32_t,uint64_t` |
| `Bool`                       | `bool`                         |
| `Void`                       | `ADL::Void`                         |
| `Float,Double`               | `float,double`                 |
| `String`                     | `std::string`                  |
| `ByteVector`                 | `ADL::ByteVector`                   |
| `Vector<T>`                  | `std::vector<T>`               |
| `StringMap<T>`               | `std::map<std::string,T>`      |
| `Nullable<T>`                | `ADL::Nullable<T>`                   |

## ADL structs

Struct declarations in ADL:

```
    struct Rectangle
    {
        Double width;
        Double height;
    };
    
```

generate C++ structs in the expected way:


```
struct Rectangle
{
    Rectangle();
    
    Rectangle(
        const double & width,
        const double & height
        );
    
    double width;
    double height;
};
```

## ADL unions

Given C++ lack of support for sum types, ADL unions:

```
    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        Translated<Picture> translated;
    };
```

map to a more complex C++ class:

```
class Picture
{
public:
    static Picture mk_circle( const Circle & v );
    static Picture mk_rectangle( const Rectangle & v );
    static Picture mk_composed( const std::vector<Picture>  & v );
    static Picture mk_translated( const Translated<Picture>  & v );
    
    Picture( const Picture & );
    ~Picture();
    Picture & operator=( const Picture & );
    
    enum DiscType
    {
        CIRCLE,
        RECTANGLE,
        COMPOSED,
        TRANSLATED
    };
    
    DiscType d() const;
    
   
    bool is_circle() const;
    bool is_rectangle() const;
    bool is_composed() const;
    bool is_translated() const;
    
    Circle & circle() const;
    Rectangle & rectangle() const;
    std::vector<Picture>  & composed() const;
    Translated<Picture>  & translated() const;
    
    const Circle & set_circle(const Circle & );
    const Rectangle & set_rectangle(const Rectangle & );
    const std::vector<Picture>  & set_composed(const std::vector<Picture>  & );
    const Translated<Picture>  & set_translated(const Translated<Picture>  & );
    
private:
   ...
};
```

## ADL newtypes

In order to preserve the ADL semantic that a newtype is an independent type
isomorphic to the original type, newtypes such as

```
    newtype Factory = String;
```

map to C++ structs with a single element:

```
struct Factory
{
    Factory() {}
    explicit Factory(const std::string & v) : value(v) {}
    
    std::string value;
};
```

## ADL type aliases

ADL type aliases

```
type T1 = Int32;
```

map directly to their corresponding C++ type aliases

```
using T1 = int32_t;
```

## Generic types

The examples above are all for monorphic ADL declarations. Polymorphic ADL declarations
map to C++ templates as would be expected.

# Runtime

The generated code depends on a small runtime, which currently has
no external dependencies. The runtime is present in the git repository
at `cpp/runtime`.

# Annotations

The c++ backend merges annotations from files with an `.adl-cpp`
suffix: eg when loading `demo/model.adl` it will automatically merge
`demo/model.adl-cpp` if found.

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
string. This would normally result in the following generated c++
code:

```
struct Date
{
    explicit DateO(const std::string & v) : value(v) {}
    
    std::string value;
};
```

The issue here is that all user written code making use of ADL values
must be prepared to convert from date strings to more appropriate
native types. A solution is to use a custom type mapping to map the
ADL Date definition to some application Date type. This means that,
in all generated c++ code, this custom type will be
used in lieu of the Date declaration shown above. The developer no
longer need to write any conversion code - all dates throughout the
ADL system use the application type of her choice.

A key thing to realise here is that, even when custom type mappings
are being used, the ADL definitions fix the serialisation of the data
types. This is what permits interoperability between processes coded
in different languages.


Custom types are specified through ADL annotations. To integrate an
arbitrary c++ type into ADL, once needs to provide:

  * the name of the custom type, along with it's associated imports
  * serialization code, that converts the custom type to/from the serialized json

Then, an annotation tells the ADL compiler to subsitute this custom
definition in place of the automatically generated one:

```
import adlc.config.cpp.CppCustomType;

annotation Date CppCustomType {
  "cppname" : "Date",
  "cppincludes" : [
      { "name" : "Date.h", "system" : false }
   ],
  "declarationCode" : ...,
  "serialisationCode" : ...,
};
```

## Standard Custom Types

There are predefined c++ custom type mappings for the following
declarations in the [adl standard library][stdlib]:

| ADL Type               | c++ Type   |
|------------------------|----------------|
| `sys.types.Pair<T1,T2>`  | `std::pair<T1,T2>` |
| `sys.types.Map<K,V>`   | `std::map<K,V>`   |
| `sys.types.Set<V>`     | `std::set<V>`    |

