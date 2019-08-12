#!/usr/bin/env bash
set -e 
set -x
FILENAME=$1
convert ${FILENAME} -flatten -fuzz 2% -trim +repage ${FILENAME}
