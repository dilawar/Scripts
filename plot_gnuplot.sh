#!/bin/bash
FILES="$@"
filename="$1_$#"
OUTFILE="$filename.png"
TERM="svg"
RANGE="1:2"
case $TERM in
    "eps") 
        TERMSTRING="postscript eps enhanced color"
        OUTFILE="$filename.eps"
        ;;
    "png")
        TERMSTRING="pngcairo"
        OUTFILE="$filename.png"
        ;;
    "svg")
        TERMSTRING="svg"
        OUTFILE="$filename.svg"
        ;;
    "*")
        TERMSTRING="png"
        OUTFILE="$filename.png"
        ;;
esac
SEP=","

SCRIPT="
set term $TERMSTRING \n
set output \"$OUTFILE\" \n
set datafile separator \"$SEP\" \n
plot 
"
for file in $@; do
    echo "Plotting $file to $OUTFILE"
    SCRIPT="$SCRIPT 
    \"$file\" using $RANGE with lines, "
done
echo -e  $SCRIPT > plot.gnuplot
gnuplot plot.gnuplot
