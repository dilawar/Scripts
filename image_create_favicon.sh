#!/usr/bin/env sh

set -x
set -e

# This is from https://stackoverflow.com/a/15104985/1805129
FILENAME="$1"
convert "$FILENAME"  -bordercolor white -border 0 \
          \( -clone 0 -resize 16x16 \) \
          \( -clone 0 -resize 32x32 \) \
          \( -clone 0 -resize 48x48 \) \
          \( -clone 0 -resize 64x64 \) \
          -delete 0 -alpha off -colors 256 favicon.ico
