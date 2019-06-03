#!/usr/bin/env bash
FILENAME=$(readlink -f $1)
DIR=$(dirname ${FILENAME})
(
    cd $DIR
    echo "IN DIR: `pwd`"
    python $(basename ${FILENAME})
)
