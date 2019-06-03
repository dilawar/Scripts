#!/usr/bin/env bash
FILENAME=$(readlink -f $1)
DIR=$(dirname ${FILENAME})
(
    cd $DIR
    python $(basename ${FILENAME})
)
