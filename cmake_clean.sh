#!/bin/bash
set -x
set -e
DIR=${1:-.}
find $DIR -iname '*cmake*' -not -regex ".*\(CMakeLists.txt$\|cmake_modules.*\)" -exec rm -rf {} \+
