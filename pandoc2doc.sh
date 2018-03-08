#!/usr/bin/env bash
#===============================================================================
#   DESCRIPTION: Generate DOCX from pandoc.
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Monday 26 February 2018 05:46:00  IST
#      REVISION:  ---
#===============================================================================

set -e
set -o nounset                                  # Treat unset variables as an error

PANDOC="$1";shift;
EXT=${1:-docx};shift;

echo "generating docx"
cat $PANDOC | $HOME/Scripts/pandoc/preprocess_of_docx.py | \
    pandoc -F $HOME/Scripts/pandoc/code_blocks.py \
    -F pandoc-crossref -F pandoc-citeproc "$@" -o $PANDOC.$EXT
