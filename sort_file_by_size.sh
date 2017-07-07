#!/usr/bin/env bash
#find . -type f -ls | sort -r -n -k7 
find . -type f  -exec du -h {} + | sort -r -h
