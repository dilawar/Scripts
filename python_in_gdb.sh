#!/bin/bash
set -x -e
if [[ $# -gt 0 ]]; then
    gdb -ex r --args python3 $@
else
    echo "Usage $0 file.py"
    exit
fi
