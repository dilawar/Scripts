#!/bin/bash
set -e
# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "USAGE: ./$0 filename.markdown [output.pdf]"
    exit
fi

LATEX="pdflatex -shell-escape"
if [ ! -d .temp ]; then
    mkdir -p .temp
fi

filename=$1
if [ $# -eq 2 ]; then
    outputFile=$2
else
    outputFile="${filename%.markdown}.pdf"
fi

texFile=${filename%.pandoc}.tex
# now convert the file to pdf
PANDOC="pandoc --data-dir=$HOME/Scripts/pandoc --filter=pandoc-citeproc --number-sections"
INFMT=markdown+tex_math_dollars+latex_macros+header_attributes+yaml_metadata_block+table_captions
echo "Converting $filename to $outputFile using pandoc"
latex="true"
if [[ $latex = "true" ]]; then
    cp *.pandoc .temp
    (
        cd .temp
        $PANDOC -s -f $INFMT -t latex -o $texFile $filename
        $LATEX  $texFile
    )
    cp .temp/*.pdf .
    cp .temp/*.tex .
else
    $PANDOC -s -f $INFMT -o $outputFile $filename
fi

