#!/bin/bash
# quick script to check that the java compiles

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

libdir=$HERE/../../../java/src/main/java
testdirs=`find $HERE -name 'java-output'`

for dir in $testdirs ; do
    echo "**" $dir 
    javafiles=`find $dir -name '*.java'`
    javac -Xlint:unchecked -d /tmp -sourcepath $libdir:$dir/../input/java $javafiles
done
