#!/bin/bash
if [[ $1 == "-d" ]]; then
    echo "Cleaning old cmake cache files"
    find . -name "*CMakeCache*" -print0 | xargs -I file rm -f file
else
    echo "Warning: Using old CMakeCache files."
fi
cmake .
make
