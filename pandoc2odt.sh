#!/usr/bin/env bash
#===============================================================================
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Monday 26 February 2018 05:46:00  IST
#      REVISION:  ---
#===============================================================================

set -e
set -o nounset                                  # Treat unset variables as an error
EXT=odt

PANDOC="$1";shift
cat $PANDOC | ./preprocess_of_docx.py | \
    pandoc -F ~/Scripts/pandoc/code_blocks.py \
    -F pandoc-crossref -F pandoc-citeproc \
    "$@" \
    -o $PANDOC.$EXT
