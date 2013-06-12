#!/bin/bash
find . -name "$1" | xargs -I file grep -Hn -e "$2" file
