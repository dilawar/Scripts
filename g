#!/bin/bash
# Find a file recursively containing a pattern
if [ $# -eq 1 ]; then
    pattern="$1"
    file_pattern=".*"
elif [ $# -eq 2 ]; then
    pattern="$1"
    file_pattern="$2"
else
    echo "Usage: #0 pattern [file_pattern]"
    exit
fi
find . -type f -regex "$file_pattern" | xargs -I file grep -rl "$pattern" file
