#!/bin/bash
# Find a file recursively containing a pattern
if [ $# -eq 1 ]; then
    pattern="$1"
    glob_pattern=".*"
    dir_pattern="."
elif [ $# -eq 2 ]; then
    pattern="$1"
    glob_pattern="$2"
    dir_pattern="."
elif [ $# -eq 3 ]; then
    pattern="$1"
    glob_pattern="$2"
    dir_pattern="$3"
else
    echo "Usage: $0 text_pattern [glob_pattern] [dir_pattern]"
    exit
fi
#echo "Searching $dir_pattern for text $pattern in files $glob_pattern"
files=`find $dir_pattern -type f -name "$glob_pattern"` 
for f in $files; do
    grep -Hnr -B 1 -A 1 "$pattern" $f
done
