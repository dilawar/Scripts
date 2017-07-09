#!/bin/bash
set -e
set -x
unset TEXMF
# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi


LATEX="lualatex -shell-escape"

filename=$1
outputFile="${filename%.pandoc}.pdf"

pandoc -s -f markdown+latex_macros+tex_math_dollars --filter=graphviz.py $filename -o $outputFile
exit;

