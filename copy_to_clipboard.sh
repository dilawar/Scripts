#!/bin/bash
if [ ! -f $1 ]; then
    echo "First argument must be a valid file path."
    echo " Given : $1"
    exit;
fi
xsel -i -b < $1
echo "Copied $1 to clipboard"
