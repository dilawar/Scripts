#!/usr/bin/env bash

WAVDIR=$(realpath "$1")
(
    cd $WAVDIR
    python3 -m libmeena.cmd classify-dir .
)
