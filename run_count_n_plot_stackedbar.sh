#!/bin/bash
set -e
set -x
if [ $# -lt 1 ]; then
    echo "USAGE : $0 datadir"
    exit;
fi

FASTAFILES=`find $1 -type f -name "*.fasta"`
for f in $FASTAFILES; do
    echo "Analyzing $f"
    ./count_nucleotide_columwise_tocsv.py $f
done

CSVFILES=`find $1 -type f -name "*.csv"`
for f in $CSVFILES; do
    echo "plotting $f"
    ./plot_stackedbar_nucleotide_csv.py $f
done
