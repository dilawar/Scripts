#!/bin/bash
if [ $# -lt 1 ]; then
    echo "USAGE: $0 url"
    exit
fi
URL=$1
LOG_FILE=__wget__log__
echo "Scanning $URL"
wget --spider -o $LOG_FILE -e robots=off -w 1 -r -p $URL
echo "Checking for broken links"
grep -B 2 '404' $LOG_FILE
