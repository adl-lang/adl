# Binary releases

As of version 0.13, binary releases for osx and linux are published on the [releases][] page. You
can use this shell script to download, cache and then run the compiler:

```
#!/bin/bash
# 
# script that downloads and caches the adl compiler if necessary, and then
# runs it.

set -e

adlversion=0.13.5

if [ "$(uname)" == "Darwin" ]; then
  platform=osx
  cachedir=$HOME/Library/Caches/adl
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  platform=linux
  cachedir=$HOME/.cache/adl 
else
  echo "Unable to download ADL for platform"
  exit 1
fi

downloads=$cachedir/downloads
release=https://github.com/timbod7/adl/releases/download/v$adlversion/adl-bindist-$adlversion-$platform.zip

if [ ! -d "$cachedir/$adlversion" ]; then
  echo "fetching $release ..."
  mkdir -p $downloads
  (cd $downloads; wget -q $release || (echo "download failed"; exit 1))
  mkdir -p $cachedir/$adlversion
  (cd $cachedir/$adlversion; unzip -q $downloads/$(basename $release))
fi

exec $cachedir/$adlversion/bin/adlc "$@"
```


# Building from source

The build process relies on [stack][].

After checking out the git repository (as `$REPO`), run the following:


```
cd $REPO/haskell
stack build --fast adl-compiler
stack install
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
