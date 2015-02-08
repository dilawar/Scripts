#!/bin/bash
set -e
# This script uses pandoc to convert pandoc to ps.. 
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "USAGE: ./$0 filename.pandoc [output.ps]"
    exit
fi

LATEX="latex -shell-escape"
if [ ! -d .temp ]; then
    mkdir -p .temp
fi

filename=$1
if [ $# -eq 2 ]; then
    outputFile=$2
else
    outputFile="${filename%.pandoc}.dvi"
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
    outfile=${outputFile%.dvi}.ps
    dvips .temp/$outputFile  -o $outfile
    cp .temp/*.tex .
else
    $PANDOC -s -f $INFMT -o $outputFile $filename
fi

