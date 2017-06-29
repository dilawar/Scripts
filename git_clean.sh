#!/usr/bin/env bash

if [ ! -d $1 ]; then
    echo "Dir $1 not found"
    exit;
fi

git clean -fXd "$@"
