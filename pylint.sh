#!/usr/bin/env bash
infile="$1"
DIRNAME=$(dirname $infile)
(
    cd $DIRNAME
    python -m pylint \
        --disable=C \
        --disable=R \
        --disable=bad-indentation \
        --disable=attribute-defined-outside-init \
        --disable=unused-argument \
        $(basename $infile)
)

