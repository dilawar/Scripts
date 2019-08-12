#!/usr/bin/env bash
set -x
set -e
INPUT="$1"
FILENAME=$(basename "$INPUT")
OUTFILE=${FILENAME}.wav
if [ ! -f $OUTFILE ]; then
    echo "Converting $INPUT to $OUTFILE"
    ffmpeg -i "$INPUT" -acodec pcm_s16le -ac 1 -ar 44100 \
        $OUTFILE
fi
