#!/usr/bin/env bash
set -e
set -x
find . -type f -name "*.dat" \
    -o -name "*.txt" \
    -o -name "*.csv" \
    -o -name "*.h5" \
    -o -name "*.png" \
    -o -name "*.pdf" \
    -size +3k | xargs -I file svn add --parents file

