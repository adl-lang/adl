# ADL java backend

```
Usage: adlc java [OPTION...] files...
  -I DIR  --searchdir=DIR            Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR            Set the directory where generated code is written
          --merge-adlext=EXT         Add the specifed adl file extension to merged on loading
          --verbose                  Print extra diagnostic information, especially about files being read/written
          --no-overwrite             Don't update files that haven't changed
          --package=PACKAGE          The language package into which the generated ADL code will be placed
          --include-rt               Generate the runtime code
          --rtpackage=PACKAGE        The java package where the ADL runtime is located
          --parcelable               Generated java code will include android parcellable implementations
          --json                     Generated java code will include gson json serialization
          --hungarian-naming         Use hungarian naming conventions
          --max-line-length=PACKAGE  The maximum length of the generated code lines
          --header-comment=PACKAGE   A comment to be placed at the start of each java file
```

# Generated Code

The java backend generates java code from the input ADL files. Each
ADL module results in a java package - individual ADL declarations
will produce a source file inside that package.

The `--package` compiler flag specifies the root package for generated
java code. Hence, an adl declaration for `SomeStruct` in module
`foo.bar` with the compile flag `--package project1.adl` would result
in the java class `project1.adl.foo.bar.SomeStruct`.

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

produce java classes (see [Rectangle.java][rect-java],
[Picture.java][pic-java]). The code for ADL structs follows standard
java conventions: private members, accessors, mutators, `hashCode()` and
`equals()`, etc.

Given the lack of sum types in java, ADL unions are compiled to a
class with a enum discriminator member, and accessors and static
constructors for each union field. The accessors will throw an
`IllegalStateException` if they are called for a field that doesn't
match the current discriminator value.

ADL newtypes are translated to java classes with a single member
variable. ADL type aliases are eliminated in the generated java code
by substitution.

Each generated java class includes static helpers to
construct:

* a [`Factory`][java-factory] for deep coping values and also
for runtime type information.

* a [`JsonBinding`][java-jsonbinding] for json serialization

# Primitive Types

The ADL primitive types are mapped to java types as follows:

| ADL Type                     | Java Type                     |
|------------------------------|-------------------------------|
| `Int8,Int16,Int32,Int64`     | `byte,short,int,long`         |
| `Word8,Word16,Word32,Word64` | `byte,short,int,long`         |
| `Bool`                       | `boolean`                     |
| `Void`                       | `Void`                        |
| `Float,Double`               | `float,double`                |
| `String`                     | `String`                      |
| `ByteVector`                 | `adl.runtime.ByteArray`       |
| `Vector<T>`                  | `java.util.ArrayList<T>`      |
| `StringMap<T>`               | `java.util.HashMap<String,t>` |
| `Nullable<T>`                | `java.util.Optional<T>`       |

The generated adl will use unboxed primitives where possible,
reverting to boxed primitives when necessary (ie `int` vs `Int`).


# Runtime

The generated code depends upon a small runtime. The location of the
runtime in the java package tree can be controlled with the
`--rtpackage` compiler flag. As a convenience, when the `--include-rt`
flag is specified, the adl compiler will also output the runtime code.

As a concrete example, if the adl compiler is called like this:

```
adlc java \
  --outputdir src \
  --package adl \
  --json \
  --rtpackage adl/runtime \
  --include-rt \
  picture.adl
```
The following files will be created:

```
src/adl/picture/Circle.java
src/adl/picture/Picture.java
src/adl/picture/Rectangle.java
src/adl/picture/Translated.java
src/adl/runtime/ByteArray.java
src/adl/runtime/DynamicHelpers.java
src/adl/runtime/Factories.java
src/adl/runtime/Factory.java
src/adl/runtime/JsonBinding.java
src/adl/runtime/JsonBindings.java
src/adl/runtime/JsonHelpers.java
src/adl/runtime/JsonParseException.java
src/adl/runtime/Lazy.java
```

The runtime itself depends on the following java packages:

* [gson](https://github.com/google/gson)

# Annotations

The java backend merges annotations from files with an `.adl-java`
suffix: eg when loading `demo/model.adl` it will automatically merge
`demo/model.adl-java` if found.

Any `Doc` annotations (which can also be specified using `///`
comments), are included as comments in the generated java code.

Generated java code normally has its root package specified by the
`--package` command line flag. The [`JavaPackage`][java-annotations]
module annotation can be used to control the root package for
individual adl modules. For example, this declaration

```
@JavaPackage "com.mycompany.adl"
module api.rest
{
...
};
```

would result in java classes being generated in package
`com.mycompany.adl.api.rest`.

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

This would normally result in a generated java class. We'd prefer
instead to use the java [`LocalDate`][java-localdate] type throughout
the ADL. The can be done with a [`JavaCustomType`][java-annotations]
annotation:

```
annotation Date JavaCustomType {
  "javaname" : "java.time.LocalDate",
  "helpers" : "helpers.DateHelpers",
  "generateType" : false
  };
```

The `javaname` property gives the fully scoped name of the type to be
used (ie `LocalDate`). The `helpers` property gives the fully scoped
name of a user provided class containing static helper functions
required to adapt the `LocalDate` class to the adl system (eg
[`helpers.DateHelpers`][java-datehelpers]). These must provide:

* A `create` function to construct literal values
* An instance of the [`Factory`][java-factory] interface
* An instance of the [`JsonBinding`][java-jsonbinding] interface

The `generateTypeProperty` controls whether the standard ADL generated
code is still emitted (even though it will be unreferenced). This can
be useful in the implementation of the custom type.

## Standard Custom Types

There are predefined java custom type mappings for the following
declarations in the [adl standard library][stdlib]:

| ADL Type           | Java Type          |
|--------------------|--------------------|
| sys.types.Map<K,V> | java.util.HashMap  |
| sys.types.Set<V>   | java.util.HashSet  |
| sys.types.Maybe<T> | java.util.Optional |

[rect-java]:../haskell/compiler/tests/demo1/java-output/adl/picture/Rectangle.java
[pic-java]:..//haskell/compiler/tests/demo1/java-output/adl/picture/Picture.java
[java-localdate]:https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
[java-annotations]:../haskell/compiler/lib/adl/adlc/config/java.adl
[java-datehelpers]:../haskell/compiler/tests/test4/input/java/helpers/DateHelpers.java
[java-factory]:../java/runtime/src/main/java/org/adl/runtime/Factory.java
[java-jsonbinding]:../java/runtime/src/main/java/org/adl/runtime/JsonBinding.java
[stdlib]:../adl/stdlib
