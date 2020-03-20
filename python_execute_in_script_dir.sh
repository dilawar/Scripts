#!/usr/bin/env bash
FILENAME=$(readlink -f $1); shift;
DIR=$(dirname ${FILENAME})
(
    cd $DIR
    echo "IN DIR: `pwd`"
    python3 $(basename ${FILENAME}) $@
)
