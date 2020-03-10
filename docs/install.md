# Building from source

The build process relies on [stack][].

After checking out the git repository (as `$REPO`), run the following:


```
cd $REPO/haskell
stack build ./compiler-bootstrap
stack install ./compiler
```

Then, you should be able to run the compiler installed into your home
directory:

```
$ ~/.local/bin/adlc --help
Usage: adlc verify [OPTION..] <modulePath>...
       adlc ast [OPTION..] <modulePath>...
       adlc haskell [OPTION..] <modulePath>...
       adlc cpp [OPTION..] <modulePath>...
       adlc java [OPTION..] <modulePath>...
$ ~/.local/bin/adlc java  --help
unrecognized option `--help'
Usage: adlc java [OPTION...] files...
  -I DIR  --searchdir=DIR            Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR            Set the directory where generated code is written
          --custom-types=FILE        Read custom type mapping from the specified file
          --no-overwrite             Don't update files that haven't changed
          --package=PACKAGE          The java package into which the generated ADL code will be placed
          --rtpackage=PACKAGE        The java package where the ADL runtime is located
          --include-rt               Generate the runtime code
          --parcelable               Generated java code will include android parcellable implementations
          --json                     Generated java code will include gson json serialization
          --hungarian-naming         Use hungarian naming conventions
          --max-line-length=PACKAGE  The maximum length of the generated code lines
          --header-comment=PACKAGE   A comment to be placed at the start of each java file
$
```

# From scratch build

```
cd $REPO/haskell
./tools/scratch-build.sh
```

[stack]: https://docs.haskellstack.org/en/stable/README/
[releases]: https://github.com/timbod7/adl/releases
