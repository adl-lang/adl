# Introduction

ADL is a framework for building multi language systems. It consists of
a domain specific language (DSL) for describing algrebraic data types,
code generators for several target languages, and runtimes for these
languages.

# Example

Here is a tiny ADL example:

```
module picture
{
    union Picture
    {
        Circle circle;
        Rectangle rectangle;
        Vector<Picture> composed;
        TranslatedPicture translated;
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

    struct TranslatedPicture
    {
        Double xoffset;
        Double yoffset;
        Picture picture;
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

Using the (experimental) `Sink` communications primitive, A variety of
[application level communication protocols][5] can be defined in the
ADL language.


[examplejava]:../haskell/compiler/tests/demo1/java-output/adl/picture/
[examplehaskell]:../haskell/compiler/tests/demo1/hs-output/ADL/Picture.hs
[1]:language.md
[2]:compiler.md
[3]:install.md
[4]:custom-types.md
[5]:serialization.md
[6]:protocols.md
