#!/usr/bin/env bash
infile="$1"
DIRNAME=$(dirname $infile)
(
    cd $DIRNAME
    python -m pylint -E \
        $(basepath $infile
)

