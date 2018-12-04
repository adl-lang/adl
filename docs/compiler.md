# 1. Synopsis

The ADL compiler reads and checks ADL definitions, and generates code
in a target programming language.

```
Usage: adlc verify [OPTION..] <modulePath>...
       adlc ast [OPTION..] <modulePath>...
       adlc haskell [OPTION..] <modulePath>...
       adlc cpp [OPTION..] <modulePath>...
       adlc java [OPTION..] <modulePath>...
       adlc javascript [OPTION..] <modulePath>...
       adlc typescript [OPTION..] <modulePath>...
       adlc show --version
       adlc show --adlstdlib
```

# 2. Usage
## verify backend

```
Usage: adlc verify [OPTION...] files...
  -I DIR  --searchdir=DIR     Add the specifed directory to the ADL searchpath
          --merge-adlext=EXT  Add the specifed adl file extension to merged on loading
```

The verify backend parses and checks the supplied ADL files, but
doesn't generate any code.

## haskell backend

See the [haskell backend guide][backend-haskell].

## java backend

See the [java backend guide][backend-java].

## typescript backend

See the [typescript backend guide][backend-typescript].

## cpp backend

```
Usage: adlc cpp [OPTION...] files...
  -I DIR  --searchdir=DIR       Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR       Set the directory where generated code is written
          --merge-adlext=EXT    Add the specifed adl file extension to merged on loading
          --verbose             Print extra diagnostic information, especially about files being read/written
          --no-overwrite        Don't update files that haven't changed
          --include-prefix=DIR  The prefix to be used to generate/reference include files
```

The cpp backend generates parses and checks the supplied ADL
files, and generates corresponding c++ code.

The C++ code generator will produce the code for module a.b in the
c++ namespace ADL.a.b, and write it to files:

        <include-prefix>/a.b.h
        a.b.cpp

C++ code is generated according to the C++11 standard.

## ast backend

```
Usage: adlc ast [OPTION...] files...
  -I DIR  --searchdir=DIR     Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR     Set the directory where generated code is written
          --merge-adlext=EXT  Add the specifed adl file extension to merged on loading
          --verbose           Print extra diagnostic information, especially about files being read/written
          --no-overwrite      Don't update files that haven't changed
```

The ast backend generates parses and checks the supplied ADL files,
and generates JSON values corresponding to the adl syntax tree. The
generated JSON matches the schema defined in stdlib/sys/adlast.adl.

# 3. Common Options

The following options are common to most code generation backends.

## searchdir

    --searchdir <dir> | -I <dir>

the --searchdir option adds a directory to the ADL search path. While
processing adl files, an import of the form:

    import a.b.c.z;

references definition z from the a.b.c module. The first file found on
the ADL search path with name a/b/c.adl will be used for this import.

##  outputdir

    --outputdir <dir> | -O <dir>

By default generated code will be written to the current
directory. This option specifies an alternate location.

## no-overwrite

    --no-overwrite

This option specifies that existing generated files should be left
untouched if the generated code is unchanged. This can be useful from
within build systems, to avoid unnecessary recompilation.

## merge-adlext

    --merge-adlext=EXT

Most commonly, an ADL module is specified in a single file. However,
the compiler has the facility to merge multiple files to construct
a single module. This is most commonly used to allow annotations for
a module to be provided in different files. By default the compiler
will search for, and merge files with the following extensions:

| adl backend | merged extension |
|-------------|------------------|
| haskell     | .adl-hs          |
| java        | .adl-java        |
| typescript  | .adl-ts          |

This means that when the haskell file reads an adl file `foo.adl` it
will also load and merge the declarations from the file `foo.adl-hs`
if that file exists.

[backend-haskell]: backend-haskell.md
[backend-java]: backend-java.md
