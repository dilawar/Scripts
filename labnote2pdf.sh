#!/bin/bash - 
#===============================================================================
#
#          FILE: labnote2pdf.sh
# 
#         USAGE: ./labnote2pdf.sh 
# 
#   DESCRIPTION:  Convert labnote to pdf.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 09/19/2016 09:03:29 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
unset TEXMF
# This script uses pandoc to convert markdown to pdf. 
if [ $# -lt 1 ]; then
    echo "USAGE: ./$0 filename.pandoc [optional]"
    exit
fi


#LATEX="pdflatex -shell-escape"
LATEX="lualatex --shell-escape"

filename=$1
outputFile="${filename%.pandoc}.pdf"

texFile=${filename%.pandoc}.tex
# now convert the file to pdf
PANDOC="pandoc --filter ~/Scripts/pandoc/image_local_copy.hs ${@:2} --data-dir=$HOME/Scripts/pandoc --number-sections"
INFMT=markdown+citations+tex_math_dollars+multiline_tables+latex_macros+header_attributes+yaml_metadata_block+table_captions
echo "Converting $filename to $outputFile using pandoc"
latex="true"
if [[ $latex = "true" ]]; then
    $PANDOC -s -f $INFMT -t latex -o $texFile $filename
    $LATEX  $texFile
else
    $PANDOC -s -f $INFMT -o $outputFile $filename
fi
