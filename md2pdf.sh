#!/bin/bash
# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "USAGE: ./$0 filename.markdown [output.pdf]"
fi

filename=$1
if [ $# -eq 2 ]; then
    outputFile=$2
else
    outputFile="${filename%.markdown}.pdf"
fi
# now convert the file to pdf
echo "Converting $filename to $outputFile using pandoc"
pandoc -f markdown+tex_math_dollars+latex_macros -o $outputFile $filename

