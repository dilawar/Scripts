#!/bin/bash
# Find a file recursively containing a pattern
if [ $# -eq 1 ]; then
    pattern="$1"
    file_pattern=".*"
    dir_pattern="."
elif [ $# -eq 2 ]; then
    pattern="$1"
    file_pattern="$2"
    dir_pattern="."
elif [ $# -eq 3 ]; then
    pattern="$1"
    file_pattern="$2"
    dir_pattern="$3"
else
    echo "Usage: $0 text_pattern [file_pattern] [dir_pattern]"
    exit
fi

echo "Searching $dir_pattern for text $text_pattern in files $file_pattern"
find $dir_pattern -type f -regex "$file_pattern" | xargs -I file \
    grep -Hnr -B 1 -A 1 "$pattern" file
