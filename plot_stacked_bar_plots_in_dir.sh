#!/bin/bash

if [ $# -lt 1 ]; then
    echo "USAGE : $0 datadir"
    exit;
fi

FASTAFILES=`find $1 -type f -name "*.fasta"`
for f in $FASTAFILES; do
    echo "Analyzing $f"
    ./plot_stacked_bar_plots_from_fasta.py $f
done
