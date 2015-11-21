#!/bin/bash
## Build using cmake.
if [[ $1 == "--clean" ]]; then
    echo "Cleaning old cmake cache files"
    find . -name "*CMakeCache*" -print0 | xargs -I file rm -f file
    exit
else
    echo "Warning: Using old CMakeCache files."
fi
mkdir -p _build
(
    cd _build
    cmake "$@" ..
    make -j`nproc`
)
