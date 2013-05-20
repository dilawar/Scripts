#!/bin/bash
find $1 -iname '*cmake*' -not -regex ".*\(CMakeLists.txt$\|.*\.cmake$\)" -exec rm -rf {} \+
