#!/bin/bash
# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "USAGE: ./$0 filename.markdown [output.pdf]"
    exit
fi

filename=$1
if [ $# -eq 2 ]; then
    outputFile=$2
else
    outputFile="${filename%.markdown}.pdf"
fi
texFile=${filename%.markdown}.tex
texFile=${filename%.pandoc}.tex
# now convert the file to pdf
PANDOC="pandoc --data-dir=$HOME/Scripts/pandoc"
echo "Converting $filename to $outputFile using pandoc"
latex="true"
if [[ $latex = "true" ]]; then
    $PANDOC -s -f markdown+tex_math_dollars+latex_macros -t latex -o $texFile $filename
    pdflatex -shell-escape $texFile
    rm -f *.log *.out *.aux
else
    $PANDOC -s -f markdown+tex_math_dollars+latex_macros -o $outputFile $filename
fi

