#!/usr/bin/env bash
set -e
INPUT=""
for FILE in $@; do
    if [ -f "$FILE" ]; then
        INPUT="${INPUT} $FILE"
        shift
    fi
done
printf "Total files: "
echo $INPUT | wc -w

TIMESTAMP=$(date +%F)
DEFAULT=animation-${TIMESTAMP}.gif

OUTPUT=${1:-$DEFAULT}
echo "Output file ${OUTPUT}"

convert \
    -fuzz 5% \
    -delay 1x8 \
    -resize 75% \
    $INPUT \
    -coalesce \
    -layers OptimizeTransparency \
    "$@" ${OUTPUT}
