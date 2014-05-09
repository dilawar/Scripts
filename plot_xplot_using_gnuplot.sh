#!/bin/bash
# Plot xplot data using gnuplot and save it to a file

if [ $# -lt 1 ]; then
    echo "USAGE: $0 filename.plot"
    exit
fi

data_file="$1"
if [ $# -eq 2 ]; then
    outfile="$2"
else
    outfile="$data_file".eps
fi
echo "Plotting $data_file and saving it to $outfile"

gnuplot << EOF
    set terminal postscript eps color solid 
    set view 80,100
    set output '${outfile}'
    plot '${data_file}' using 1:2 with lines 
EOF

