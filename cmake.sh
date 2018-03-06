#!/bin/bash

## Build using cmake.
set -e -x

if [[ $1 == "--clean" ]]; then
    echo "Cleaning old cmake cache files"
    find . -name "*CMakeCache*" -print0 | xargs -I file rm -f file
    exit
else
    echo "Warning: Using old CMakeCache files."
fi

mkdir -p _build
GITTAG=$(git rev-parse --short HEAD)
(
    cd _build
    cmake -DCMAKE_INSTALL_PREFIX=/tmp/$GITTAG .. "$@"
    make -j`nproc` | tee __$GITTAG__.log
    ctest --output-on-failure
    make install
)
