# Overview

ADL (Algebraic Data Language) is the domain specific language at
the core of the ADL framework. It is used to describe application
level data types and communication protocols.

The language supports:

   * a data model based upon [algebraic data types](http://en.wikipedia.org/wiki/Algebraic_data_types)

   * parameterised data types

   * field defaulting with comprehensive literal support.

   * typed annotations

Together these features facilitate the interoperation between a
variety of object oriented and functional programming languages.  The
syntax of ADL is straightforward, and will be familiar to java and c++
developers.

# Modules

The ADL namespace is managed through modules - all definitions exist
with a module. Modules are hierarchical, and tied to the directory
structure in the manner of java and haskell. Hence the ADL for module
"demos.ping" must be stored in a file "demos/ping.adl".

A module consists of a module header, an optional list of import
declarations, and then a list of type definitions:

    module demo.sample
    {
        import sys.types.*;
        import demo.types.Foo;

        ... type definitions ...
    };

Import declations bring type definitions from other modules into the
scope of the current module. An import statement may bring a single
definition into scope, or the entire contents of another module using
'*'.

Types defined in other modules may also be referenced though the use
of fully scoped names.

Recursive definitions and mutually recursive definitions are allowed
with a single module, but may not span across modules.

# Primitive Types

ADL supports the following primitive types:

| Type                         | Description                                            |
|------------------------------|--------------------------------------------------------|
| Int8,Int16,Int32,Int64       | Signed integers                                        |
| Word8,Word16,Word32,Word64   | Unsigned integers                                      |
| Bool                         | boolean values                                         |
| Void                         | The unary or "null" type                               |
| Float,Double                 | floating point values                                  |
| String                       | A unicode text string                                  |
| ByteVector                   | A packed array of bytes                                |
| `Vector<T>`                  | A vector/array of type T                               |
| `StringMap<T>`               | A map with string keys and values of type T            |
| `Nullable<T>`                | An optional value                                      |

# Type Definitions

There are 4 varieties of type definitions: structs, unions, typedefs and newtypes.

## struct

A struct definition specifies a record in the conventional way, ie as
a list of fields with associated types:

    struct Person
    {
        String firstName;
        String lastName;
        Int16 age;
        Gender gender;
    };

In terms of algebraic data types, a struct definition creates new "product types".

A struct definition may take type parameters. A simple example from the standard library:

    struct Pair<T1,T2>
    {
        T1 v1;
        T2 v2;
    };


## union

A union definition specifies a discriminated union (ie an algebraic sum
type). Other than a different keyword, it has identical syntax to a
struct definition:

    union StringOrDouble
    {
        String stringV;
        Double doubleV;
    };

Using the Void primitive type, unions are used to define enumerations:

    union Gender
    {
         Void male;
         Void female;
    };

A union definition may take type parameters. A simple example from the standard library:

    union Either<T1,T2>
    {
        T1 left;
        T2 right;
    };

## typedef

Typedefs are used to define type synonyms, and may take type parameters:

    type UserVec = Vector<Person>;
    type RpcSvc<I,O> = Sink<Rpc<I,O>>;

## newtype

Newtypes define a new type that is structurally equivalent to an
existing type, but has a distinct type in the generated code. newtype
definitions may also take type parameters:

    newtype UserId = String;
    newtype Map<K,V> = Vector<Pair<K,V>>;

# Standard Library

In addition to the builtin language primitives the [ADL standard library][stdlib]
defines the following:

| Module    | Type        | Description                                                                    |
|-----------|-------------|--------------------------------------------------------------------------------|
| sys.types | Pair<A,B>   | A value containing both an A value and a B value                               |
| sys.types | Either<A,B> | A value containing either an A value or a B value                              |
| sys.types | Maybe<A>    | An optional value of type A                                                    |
| sys.types | Map<K,V>    | A map with keys of type K and values of type V                                 |
| sys.types | Set<A>      | A set of values of type A                                                      |

Where there is natural support for these types in a target language or
it's standard library, appropriate custom mappings are used.

[stdlib]:https://github.com/timbod7/adl/blob/master/adl/stdlib/sys/types.adl

# Default Values

Default values can specified for fields. Such fields then become optional
in serialized values, and hence facilitate backwards compatibility. Default
literal values are specified in appropriate places in
the ADL definitions. A literal value is specified in JSON form, structured
according to the json [serialization specification](serialization.md).

## struct default overrides

A struct definition may override the default values of any or all of
it's fields:

    struct A
    {
         Int x = 1;
         String y = "hi";
         Pair<Double,Double> z = { "v1" : 1.0, "v2" : 2.0 };
    };

## union default overrides

A union definition may override the default value of a single discriminator. This
discriminator then becomes selected for the default:
it's fields:

    union A
    {
         Int x;
         String y = "hi";         // the default discriminator
         Pair<Double,Double> z;
    };

## newtype default overrides

A newtype definition may override the default value through a
(slightly awkward) "double equals" syntax:

    newtype Date = String = "2000-01-01";

# Annotations

When generating code, an ADL backend or custom code generator will
often need additional information about a type or
declaration. Annotations serve this purpose, by associating a typed
value with a module, declaration or field.

Annotation types are declared as any other ADL type. Consider
for example, annotations describing how a type is to be mapped
into a relational database:

```
module demo.dbannotations {

struct DbTable
{
  String tableName;
  Vector<String> primaryKey;
};

type DbField = String;
};
```

Then, annotation values can be associated with ADL definitions. The
values are json, using the ADL serialization schema.  The values
provided for annotations are checked againt the annotation types, and
the compilation will fail if they are incorrect. As shown below, there
are two annotation syntaxes. They can be provided as prefixes, using
the `@` syntax, or via an explicit annotation declaration.

```
module demo.model
{

import demo.dbannotations.*;

@DbTable {
  "tableName" : "demo.users",
  "primaryKey" : ["email"]
}
struct User
{
   @DbField "email"
   String email;

   @DbField "full_name"
   String fullName;
};

struct Address
{
   Int id;
   Vector<String> details;
};

annotation Address DbTable {
  "tableName" : "demo.address",
  "primaryKey" : ["id"]
};
annotation Address.id DbField "id";
annotation Address.details DbField "details";

};
```

Whilst either syntax can be used, prefix annotations can be useful for
short annotations (typically of primitive types), whilst explicit
declarations are preferable for more complex types.

Explicit annotations have the benefit that they can be stored in files
other than the main adl source. This is desirable when a given ADL
definition has many annotations (for different language backends,
special purpose processing etc). Whenever an ADL file is parsed, the
compiler will look for additional files containing annotations, and,
if found, they will be merged into the ADL source. By default the
compiler will look for annotations with a backend specific file
suffix: eg, when loading `demo/model.adl` the java backend will try to
load `demo/model.adl-java`. Additional extensions can be added to the
search with the `--merge-adlext` command line flag.

## Builtin annotation types

The ADL system understands several builtin annotation types.

### `sys.annotations.Doc`

The `Doc` annotation associates a documentation string with an ADL
module, declaration or field. There is special syntax support for
this: comments that being with triple slashes (ie `///`) are
automatically promoted to `Doc` annotations. The standard annotation
syntax is still supported. The `Doc` strings are used by language
backends to comment the generated code.

### `sys.annotations.SerializedName`

Normally the when serializing unions and structures, the adl field
name is used. The `SerializedName` field annotation overrides this.
This is useful where shortened names are required; and also when the
name of an ADL field is changed, but the serialized form must be
preserved.

### `HaskellCustomType`, `JavaCustomType`, `CppCustomType`

Annotations are used to specify custom type mappings. Refer to the
language specific backend for details.

# Naming Conventions

Identifiers are used for module names, type names, and field
names. Identifiers in ADL are case sensitive, and must match the
following regular expression:

      [A-Za-z][A-Za-z0-9_]*

The reserved words are:

      module
      import
      struct
      union
      type
      newtype

Whilst user defined names can follow any case convention, the
following conventions are used for primitive types and the ADL
standard library, and are recommended:

  1. Module names consist solely of lower case letters and numbers,
     dot separated to indicate the hierarchy.
  2. Type names start with an upper case letter, and follow
     CamelCase conventions.
  3. Field names start with a lower case letter, and follow
     camelCase conventions.
