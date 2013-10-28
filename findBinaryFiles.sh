#!/bin/bash
if [ $# -eq 0 ]; then 
  echo "Usage : findBinaryFiles.sh <dir>"
  exit
fi
# -m 1 makes sure that we stop reading as soon as we match our pattern in file.
grep -r -m 1 ^ $1 | grep "^Binary file"
