# Introduction

ADL (Algebraic Data Language) is a framework for building multi
language systems. It consists of a domain specific language (DSL) for
describing algebraic data types, code generators for several target
languages, and runtimes for these languages. Language interoperability
is ensured through a consistent json serialisation schema.

In addition to supporting several target languages, a general purpose
"ast" backend is available. This produces a checked, machine readable
syntax tree suitable for custom processing and code generation in any
programming language.

# Applications

Intended applications for ADL include:

* strongly typed http APIs
* strongly typed configuration files
* automatically derived database schemas and binding code
* automatically derived UI code for editing complex values


# Example

Here is a small ADL example:

```
module picture
{
    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        Translated<Picture> translated;
    };

    struct Circle
    {
        Double radius;
    };

    struct Rectangle
    {
        Double width;
        Double height;
    };

    struct Translated<T>
    {
        Double xoffset = 0;
        Double yoffset = 0;
        T object;
    };
};
```

This specifies a data type representing a Picture. In this example a
picture composes Rectangles and Circles, with arbitrary translations.

The adl compiler can generate the code corresponding to this data model
in any of the target languages. For example, this command

```
adlc java --json picture.adl
```

will generate [these java classes][examplejava] (which include json
serialization functions). The command to generate the corresponding
[haskell types][examplehaskell] is

```
adlc haskell picture.adl
```

# More information

The [ADL domain specific language][1] is the core of the system, and
provides a fully typed specification for data.

The [ADL compiler][2] is used to generate code from ADL source for a
variety of target languages. Currently the the compiler is built from
source - follow [these instructions][3].

A common problem with systems that rely on code generation is the
"impedance mismatch" between the generated code and the actual
application logic. The ADL compiler addresses this by supporting code
generation in terms of the application logics own types. This feature
is called [custom type mappings][4].

Currently, json serialization is supported, with a [strict mapping][5]
defined between ADL types and json values.

[examplejava]:../haskell/compiler/tests/demo1/java-output/adl/picture/
[examplehaskell]:../haskell/compiler/tests/demo1/hs-output/ADL/Picture.hs
[1]:language.md
[2]:compiler.md
[3]:install.md
[4]:custom-types.md
[5]:serialization.md
