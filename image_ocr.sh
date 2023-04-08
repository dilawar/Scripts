#!/usr/bin/env bash
set -e
FILENAME="$1"; shift;
tesseract "$FILENAME" /tmp/output -l eng $@
sed -e /^\ *$/d /tmp/output.txt
