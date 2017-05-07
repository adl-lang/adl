# Overview

ADL (Algebraic Data Language) is the domain specific language at
the core of the ADL framework. It is used to describe application
level data types and communication protocols.

The language:

   * uses a data model based upon [algebraic data types](http://en.wikipedia.org/wiki/Algebraic_data_types)

   * supports parameterised data types

   * supports field defaulting with comprehensive literal support.

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
| `Vector<T>`                  | A vector/array of type T                               |
| `StringMap<T>`                | A map with string keys and values of type T           |

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

