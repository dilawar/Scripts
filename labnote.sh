#!/usr/bin/env bash

# This script opens the labnote.
dir=_labnote
filename=$dir/experiment.pandoc
if [ ! -d $dir ]; then 
    mkdir -p $dir
fi
vim $filename
