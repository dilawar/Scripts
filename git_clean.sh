#!/usr/bin/env bash

if [ ! -d $1 ]; then
    echo "Dir $1 not found"
    exit;
fi

git clean -fxd -e vendor -e _data -e *.dat -e *.csv \
    -e *.h -e *.cpp -e *.c -e *.hpp \
    -e *.py \
    "$@"
