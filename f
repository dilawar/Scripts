#!/bin/bash
# Find a file in directory, if not given then search in current dir
if [ $# -eq 1 ]; then
    file="*$1*"
    dir="."
elif [ $# -eq 2 ]; then
    dir="$1"
    file="$2"
else
    echo "Usage: f [dir] file_pattern"
    exit
fi
find $dir -type f -name "$file"
