#!/usr/bin/env bash
set -e
INPUT=""
for FILE in $@; do
    if [ -f "$FILE" ]; then
        INPUT="${INPUT} $FILE"
        shift
    fi
done
OUTPUT=${1:-animation.gif}
printf "Total files: "
echo $INPUT | wc -w
echo "Output file ${OUTPUT}"
convert \
    -fuzz 5% \
    -delay 1x8 \
    -resize 75% \
    $INPUT \
    -coalesce \
    -layers OptimizeTransparency \
    "$@" ${OUTPUT}
