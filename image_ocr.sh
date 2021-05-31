#!/usr/bin/env bash
set -e
FILENAME="$1"
tesseract "$FILENAME" /tmp/output -l eng+hin
cat /tmp/output.txt
