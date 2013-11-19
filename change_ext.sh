#!/bin/bash
# This script rename a given file. First argument to this file is the extension
# which we want to replace, and the second argument is the new extension.
if [ $# -lt 2 ]; then 
  echo "Usage: $(basename $0) from_ext to_ext"
  exit
fi
from=$1
to=$2
files=$(find . -name "*.$1" -type f)
for f in $files
do 
  dirName=$(dirname $f)
  baseName=$(basename $f)
  newFileName="$dirName/${baseName/$from/$to}"
  echo "Copying $f to $newFileName"
  cp $f $newFileName
done
