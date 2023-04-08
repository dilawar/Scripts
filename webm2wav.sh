#!/usr/bin/env bash

INFILE="$1"
OUTFILE=${INFILE%webm}wav

if [ -f "$OUTFILE" ]; then
    mkdir -p backup
    mv "$OUTFILE" backup/
fi
ffmpeg -i "$INFILE" -ac 1 -f wav -vn -ar 20500 "$OUTFILE"
