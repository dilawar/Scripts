#!/bin/bash - 

# Overwrite the datafile after sampling every nth line.

set -o nounset                              # Treat unset variables as an error

if [[ $# -lt 2 ]]; then
    echo "USAGE: $0 filename nth"
    exit
    #statements
fi
FILE="$1"
NTH="$2"

# Don't miss the header.
TMPFILE=/tmp/__temp.dat
gawk "NR == 1 || NR % ${NTH} == 0" $FILE  > ${TMPFILE}
mv ${TMPFILE} ${FILE}
