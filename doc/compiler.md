# 1. Synopsis

The ADL compiler reads and checks ADL definitions, and generates code
in a target programming language.

    adlc verify [OPTION...] <adlfile>..
    adlc haskell [OPTION...] <adlfile>..
    adlc cpp [OPTION..] <adlfile>..
    adlc ast [OPTION..] <adlfile>..

# 2. Usage
## verify backend

    adlc verify [OPTION...] <adlfile>..

The verify backend parses and checks the supplied ADL files, but
doesn't generate any code. It supports the following options:

    --searchdir <dir> | -I <dir>

## haskell backend

    adlc haskell [OPTION...] <adlfile>..

The haskell backend generates parses and checks the supplied ADL
files, and generates corresponding haskell code. It supports the
following options:

    --searchdir <dir> | -I <dir>
    --outputdir <dir> | -O <dir>
    --no-overwrite
    --custom-types <file>
    --moduleprefix <prefix>

When generating haskell code, the module prefix is used to control
where in the haskell namespace hierarchy the generated code sits. For
example, if the module prefix is "MyApp.adl", the code for adl module
a.b will be generated with a haskell module declaration:

    module MyApp.adl.a.b

## cpp backend

    adlc cpp [OPTION..] <adlfile>..

The cpp backend generates parses and checks the supplied ADL
files, and generates corresponding c++ code. It supports the
following options:

    --searchdir <dir> | -I <dir>
    --include-prefix <prefix>
    --outputdir <dir> | -O <dir>
    --no-overwrite
    --custom-types <file>

The C++ code generator will produce the code for module a.b in the
c++ namespace ADL.a.b, and write it to files:

        <include-prefix>/a.b.h
        a.b.cpp

C++ code is generated according to the C++11 standard.

## ast backend

    adlc ast [OPTION..] <adlfile>..

The ast backend generates parses and checks the supplied ADL files,
and generates JSON values corresponding to the adl syntax tree. The
generated JSON matches the schema defined in stdlib/sys/adlast.adl.
The ast backend supports the following options:

    --searchdir <dir> | -I <dir>

# 3. Common Options

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

## custom-types

    --custom-types <file>

Certain backends allow user specified "custom" types to be used in
place of those generated in the standard way from ADL.  This option
makes available custom type definitions to the compiler. The files are
in JSON, with schemas given by:

* *haskell* : adlc/config/haskell.adl
* *c++*     : adlc/config/cpp.adl

    








