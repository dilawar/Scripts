#!/bin/bash
dotfile="$1"
outfile="$dotfile".png
neato -Tpng $dotfile > $outfile
quiet eog $outfile
