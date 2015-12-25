#!/bin/bash

# Convert any image file to png.

for FILE in "$@"; do
    if file --mime-type $FILE | grep -i image/  
    then 
        OUTFILE=${FILE%\.*}.png
        echo "[INFO] converting $FILE --> $OUTFILE"
        convert $FILE $OUTFILE
    fi
done
