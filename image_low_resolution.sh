#!/usr/bin/env bash
# Convert image to low resolution.

filename=$(basename -- "$1")
EXT="${filename##*.}"
filename="${filename%.*}"
ORIGINAL_SIZE=$(stat -c%s "$1")

OUTFILE=${filename}.low.${EXT}

convert -format ${EXT} \
    -strip -interlace Plane -gaussian-blur 0.05 -quality 85% \
    -density 72 \
    -flatten "$1" "${OUTFILE}"

# Now check the size.
ORIGINAL_SIZE=$(stat -c%s "$1")
OUTFILE_SIZE=$(stat -c%s "${OUTFILE}")
SIZE_CHANGE=$(echo "$OUTFILE_SIZE/${ORIGINAL_SIZE}" | bc -l)

echo "\t Original size ${ORIGINAL_SIZE}, output size ${OUTFILE_SIZE}"
echo "\t SIZE CHANGE: ${SIZE_CHANGE}"
