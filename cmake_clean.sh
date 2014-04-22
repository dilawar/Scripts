#!/bin/bash
find $1 -iname '*cmake*' -not -regex ".*\(CMakeLists.txt$\|cmake_modules\)" -exec rm -rf {} \+
