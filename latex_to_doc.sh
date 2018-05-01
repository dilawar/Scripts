#!/usr/bin/env bash
set -e
pandoc -f latex "$1" -t odt "$1.odt"
