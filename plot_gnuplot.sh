#!/bin/bash
filename="$1"
echo "plotting $filename"
TERM=eps
SEP=","

plot ()
{
    gnuplot <<EOF
        set xlabel $2
        set ylabel $3
        set term $TERM
        set datafile separator "$SEP"
        set output "$filename.$TERM"
        plot "$filename" using 1:$1 with lines
EOF
}
plot 2
