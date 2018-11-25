#!/usr/bin/env bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS=$($SCRIPT_DIR/pandoc_find_filters.sh)
PANDOC="pandoc -s -N $PANDOC_FILTERS "

# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.md [--tex|-tex] [pandoc args]"
    exit
fi

filename=$1; shift

if [[ "$@" == *"-tex"* ]]; then
    shift;
    outputFile="${filename}.pdf"
    outTex="${filename}.tex" 
    $PANDOC  $filename "$@" -o $outTex 

    if latexmk -pdf -lualatex -shell-escape -silent $outTex; then
        echo "Successfully built"
    else
        echo "Trying again with warning enabled"
        lualatex --shell-escape $outTex
    fi
else
    $PANDOC --pdf-engine=lualatex --pdf-engine-opt=-shell-escape $filename -o ${filename}.pdf
fi
