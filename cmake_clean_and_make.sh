#!/bin/bash
# This scripts removes all CMakeCache.txt files and re-run cmake 
find . -type f -name "*CMakeCache.txt" -print0 | xargs -0 -I file rm -f file
cmake . && make
