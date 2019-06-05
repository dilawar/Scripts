#!/usr/bin/env bash

set -e
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

PANDOC="pandoc -N \
    --template ${SCRIPT_DIR}/pandoc/templates/default.beamer \
    -F pandoc-crossref \
    "

# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi

filename="$1"
outputFile="${filename%.pandoc}.pdf"
$PANDOC -tbeamer $filename -o $outputFile
# lualatex --shell-escape ${outputFile}
