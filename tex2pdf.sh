#!/bin/bash
# This script converts tex file to pdf file. All temp files are stored in .tex
# directory.
set -e
TEMP=.temp
TEXFILE="$1"
mkdir -p $TEMP
echo "Running in $TEMP"
cp -r * $TEMP
( 
echo "Running pdflatex on tex file"
cd $TEMP && pdflatex $TEXFILE 
cp ${TEXFILE%tex}pdf ../ 
)
