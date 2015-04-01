#!/usr/bin/env bash

# This script opens the labnote.
dir=_labnote
filename=$dit/experiment.note
if [ -d $dir ]; then 
    mkdir -p $dir
fi
vim $filename
