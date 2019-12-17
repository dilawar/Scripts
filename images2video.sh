#!/usr/bin/env bash
# convert png files to open video format.
set -x
set -e
echo "First argument to this script is filename pattern"
PAT="$1"
ffmpeg -pattern_type glob -i "$PAT" -vcodec libtheora \
    -b 9000k \
    -framerate 10\
    -r 10\
    -pix_fmt yuv444p \
    output-$(date +%F).ogv
