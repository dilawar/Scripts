#!/bin/bash
dotfile="$1"
outfile="$dotfile".png
DOT=${2-dot}
$DOT -Tpng $dotfile > $outfile
#quiet eog $outfile
