#!/bin/bash
if [ $# -eq 0 ]; then
  echo "Usage : findAsciiFiles <dirname>"
  exit
fi
find $1 -type f -print0 | xargs -0 file | grep ASCII | cut -d: -f1
