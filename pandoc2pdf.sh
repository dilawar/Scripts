#!/bin/bash

set -e
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

PANDOC="pandoc -S -s -N \
    --template ${SCRIPT_DIR}/pandoc/templates/default.latex \
    -F pandoc-crossref -F pandoc-citeproc -F $SCRIPT_DIR/pandoc/siunitx.py "

# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi

filename=$1
outputFile="${filename%.pandoc}.pdf"
outTex="${filename%.pandoc}.tex"
$PANDOC -tlatex $filename -o $outTex
lualatex --shell-escape $outTex
