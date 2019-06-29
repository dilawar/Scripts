#!/usr/bin/env bash
set -x
set -e
FILENAME="$1"
convert ${FILENAME} -fuzz 20% -transparent white ${FILENAME}.png
