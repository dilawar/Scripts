#!/bin/bash
echo "Searching for $1 in /usr"
find /usr -name "*.so" -exec nm --print-file-name --defined-only \
    --dynamic {} \; | grep "$1"

find /lib -name "*.so" -exec nm --print-file-name --defined-only \
    --dynamic {} \; | grep "$1"
