#!/usr/bin/env bash
set -e
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS=$($SCRIPT_DIR/pandoc_find_filters.sh)
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

## In two-column mode, longtable does not work see https://github.com/jgm/pandoc/issues/1023
## We use a sed script to replace longtable with supertable
## This is not a great solution but it does work.
#sed -i 's/\\begin{longtable}/\\begin{supertabular}/g' $outTex
#sed -i 's/\\end{longtable}/\\end{supertabular}/g' $outTex
#sed -i 's/\\endhead//g' $outTex

if latexmk -pdf -lualatex -shell-escape -silent $outTex; then
    echo "Successfully built"
else
    echo "Trying again with warning enabled"
    lualatex --shell-escape $outTex
fi
