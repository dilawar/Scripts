#!/bin/bash
set -e
set -x
unset TEXMF
PANDOC="pandoc -S -s -N -F pandoc-citeproc -F pandoc-crossref "

# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi

filename=$1
outputFile="${filename%.pandoc}.pdf"
$(PANDOC) -tlatex $filename -o $outputFile
