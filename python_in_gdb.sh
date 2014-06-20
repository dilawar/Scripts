#!/bin/bash
if [[ $# -gt 1 ]]; then
    gdb -ex r --args python "$1" $2 $3 $4 $5 $6 $7 $8 $9
else
    echo "Usage $0 file.py"
    exit
fi
