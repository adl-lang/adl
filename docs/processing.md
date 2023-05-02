This describes the phases of ADL processing from initial parsing through to being validated and ready for code generation.

# Phases

## 1. parse module adl file(s)
A single adl file is expected to be called X.adl, where X is the module hierarchy.  Confirm the the module name in the file matches the directory name. Also load any extra files with name X.adl_Y, according to configuration. A files named X.adl_Y normally contain language Xspecific declarations or annotations to be merged into model X.  Hence these should be merged into a single module for further processing.

## 2. apply explicit annotations
All annotations are associated with a module, declaration,  or field. However they can be specified either as a prefix, or as a free floating *explicit annotation*. This phase attaches all explicit annotations to their associated element of the parse tree, and reports an error if this cannot be done.

## 3. check decl versioning
Ensure that for all the decls associated with a name, either
  * we have one unversioned decl
  * we have have a consistently versioned set

## 4. lift serialized names
Serialised names are a first class item in the AST, by default equal to the field names, but are overriden by the `@SerializedName` attribute

## 5. check for duplicates
Raise an error if there are any duplicate declarations in a module, or any duplicate fields in a struct or union, or any deplicate type parameters in a generic declaration.

## 6. resolution
Useful further interpretatiion of the ADL cannot be done until external references are resolved. This phase involves the following stages:

* find the name of all modules  referenced from the current module, by looking at imports and fully scoped references.
* resolve each of those modules, storing the results of each
* Add in default imports (ie `sys/annotations.adl`)
* rewrite the current module by deciding the kind of each type ref (either a primitive, a decl in another module, or a type parameters)
* fail if any type ref in the current module cannot be resolved.

Note the above process is recursive, and a error should be generated in the case of mutually recursive modules.

## 7. check type constructor applications
Check that every usage of a type constructure is passed the correct number of parameters according to it's declaration.

## 8. check default values
Confirm that:
* there are no default overrides for parameterised types
* the JSON literal for each default override has the appropriate type

## 9. check annotations
Confirm that the JSON literal for each annotation has the correct type.






