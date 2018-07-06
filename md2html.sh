#!/usr/bin/env bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS=$($SCRIPT_DIR/pandoc_find_filters.sh)
PANDOC="pandoc -s -N $PANDOC_FILTERS --lua-filter $SCRIPT_DIR/tikz2pdf.lua"

filename=$1; shift
outputFile="${filename}.html"
$PANDOC $filename "$@" -t html --self-contained -o $outTex 
