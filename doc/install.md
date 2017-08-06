# Binary releases

Binary releases for certain platforms are [available in github][releases]. These
include the compiler binary itself, the adl standard library and
various supporting files:

```
$ unzip -l adl-0.1.1-darwin.zip
Archive:  dist/adl-0.1.1-darwin.zip
  Length     Date   Time    Name
 --------    ----   ----    ----
        0  11-28-16 23:45   bin/
 14529244  11-28-16 23:45   bin/adlc
        0  11-28-16 23:45   lib/
        0  11-28-16 23:45   lib/adl/
        0  11-28-16 23:45   lib/adl/adlc/
        0  11-28-16 23:45   lib/adl/adlc/config/
      618  11-28-16 23:45   lib/adl/adlc/config/cpp.adl
      465  11-28-16 23:45   lib/adl/adlc/config/haskell.adl
      378  11-28-16 23:45   lib/adl/adlc/config/java.adl
        0  11-28-16 23:45   lib/adl/sys/
     1468  11-28-16 23:45   lib/adl/sys/adlast.adl
      165  11-28-16 23:45   lib/adl/sys/annotations.adl
      289  11-28-16 23:45   lib/adl/sys/rpc.adl
      575  11-28-16 23:45   lib/adl/sys/sinkimpl.adl
      572  11-28-16 23:45   lib/adl/sys/types.adl
...
```

The archive can be unpacked anywhere - the compiler binary assumes it
can find the necessary files at the appropriate relative path.

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

[stack]: https://docs.haskellstack.org/en/stable/README/
[releases]: https://github.com/timbod7/adl/releases
