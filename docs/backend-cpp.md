# ADL C++ backend

```
Usage: adlc cpp [OPTION...] files...
  -I DIR  --searchdir=DIR       Add the specifed directory to the ADL searchpath
  -O DIR  --outputdir=DIR       Set the directory where generated code is written
          --merge-adlext=EXT    Add the specifed adl file extension to merged on loading
          --verbose             Print extra diagnostic information, especially about files being read/written
          --no-overwrite        Don't update files that haven't changed
          --include-prefix=DIR  The prefix to be used to generate/reference include files
          --exclude-relops      Exclude generated code for relational operators
```
