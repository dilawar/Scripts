#!/usr/bin/env bash
set -e
# This scripts accept a literate code written in anansi and produce both
# documentation and code out of it. The documentation is in pandoc while the 
if [ $# -lt 1 ]; then
    echo "Missing input file name";
    exit;
fi
inputFile="$1"
anansi weave $inputFile -o $inputFile.pandoc
anansi tangle $inputFile 
pandoc -f markdown $inputFile.pandoc -o $inputFile.pdf
