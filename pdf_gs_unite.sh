#!/usr/bin/env bash
# Unite PDFs using ghostscript.
set -u -x -e
OUTPUTFILE="$1"; shift;
gs -q -dNOPAUSE -dBATCH -sDEVIVE=pdfwrite -sOutputFile=$OUTPUTFILE $@
