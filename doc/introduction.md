ADL is a framework for building multi language systems. It consists of
a domain specific language (DSL) for describing algrebraic data types,
code generators for several target languages, and runtimes for these
languages.

The [ADL domain specific language][1] is the core of the system, and
provides a fully typed specification for data values, independent of
the code implementing these services.

The [ADL compiler][2] is used to generate code from ADL source for a
variety of target languages. 

A common problem with systems that rely on code generation is the
"impedance mismatch" between the generated code and the actual
application logic. The ADL compiler addresses this by supporting code
generation in terms of the application logics own types. This feature
is called [custom type mappings][3].

Using the (experimental) `Sink` communications primitive, A variety of
[application level communication protocols][4] can be defined in the
ADL language.

[1]:language.md
[2]:compiler.md
[3]:custom-types.md
[4]:protocols.md
