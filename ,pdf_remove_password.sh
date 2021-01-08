#!/usr/bin/env bash
INFILE="$1"
OUTFILE="${INFILE%.pdf}-unsecured.pdf"
PASSWORD="$2"
if [[ -z "$PASSWORD" ]]; then
    echo Password:
    read -rs PASSWORD
fi
echo "Converting $INFILE to $OUTFILE"
pdftk "$INFILE" input_pw "$PASSWORD" output "$OUTFILE"
