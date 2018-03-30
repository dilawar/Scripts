#!/usr/bin/env bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS= -F $SCRIPT_DIR/pandoc/dilawar.py 

if [ -x "$(pandoc-citeproc -v)" ]; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-citeproc"
else
    echo "[WARN] pandoc-citeproc not found"
fi
if [ -x "$(pandoc-crossref -v)" ]; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-crossref"
else
    echo "[WARN] pandoc-crossref not found"
fi

if which pandoc-imagine > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-imagine"
else
    echo "[WARN] pandoc-imagine not found"
fi

if which pantable > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pantable"
else
    echo "[WARN] pantable not found"
fi


PANDOC="pandoc -s -N $PANDOC_FILTERS"

# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi

filename=$1; shift
outputFile="${filename%.pandoc}.pdf"
outTex="${filename%.pandoc}.tex" 
$PANDOC $filename "$@" -o $outTex 
latexmk -pdf -lualatex -shell-escape -silent $outTex || lualatex --shell-escape $outTex
