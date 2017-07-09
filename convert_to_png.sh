#!/bin/bash
set -x

# Convert any image file to png.

function convert2png 
{
    FILE="$1"
    OUTFILE=${FILE%\.*}.png
    echo "[INFO] converting $FILE --> $OUTFILE"
    convert $FILE $OUTFILE
}


for FILE in "$@"; do
    if file --mime-type $FILE | grep -i image/  
    then 
        convert2png $FILE
    elif file --mime-type $FILE | grep -i application/pdf 
    then
        convert2png $FILE
    fi
done
