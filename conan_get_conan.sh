#!/usr/bin/env bash
set -e -x

mkdir -p cmake
(
    cd cmake 
    wget https://raw.githubusercontent.com/conan-io/cmake-conan/master/conan.cmake
)

python3 -m pip install conan --user
