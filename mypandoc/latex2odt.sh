#!/usr/bin/env bash
set -e
pandoc -f latex  -t odt -o "$1.odt" "$1"
