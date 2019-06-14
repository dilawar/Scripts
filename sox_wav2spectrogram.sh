#!/usr/bin/env bash
# Generate spectrogram out of WAVE file using SOX.

set -e
FILENAME="$1"; shift
if [ ! -f "${FILENAME}" ]; then
    echo "${FILENAME} not found."
    exit;
fi

# NOTE: See https://sourceforge.net/p/sox/mailman/message/34749849/
OUTFILE=${FILENAME}.png
sox ${FILENAME} -r 44100 -n rate 12k spectrogram $@ -o ${OUTFILE}
echo "Wrote spectrogram to ${OUTFILE}"
