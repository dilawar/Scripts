#!/bin/bash
set -x
set -e
FILENAME=$1
ANGLE=$2
OUTFILE=$3
convert -distort SRT $ANGLE "$FILENAME" $OUTFILE
