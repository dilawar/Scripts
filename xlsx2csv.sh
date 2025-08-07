#!/usr/bin/env bash

set -e
set -x

echo "Converting $1 to $1.csv"
uvx run in2csv "$1" "$1.csv"
