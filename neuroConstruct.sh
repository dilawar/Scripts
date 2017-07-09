#!/bin/bash
set -e
ncDir=/opt/neuroConstruct
if [ ! -d $ncDir ]; then
    echo "I can't find $ncDir. Make sure neuroConstruct is kept here"
    exit
fi
cd $ncDir && ant run &
