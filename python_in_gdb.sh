#!/bin/bash
if [[ $# -eq 1 ]]; then
    gdb -ex r --args python "$1"
else
    echo "Usage $0 file.py"
    exit
fi
