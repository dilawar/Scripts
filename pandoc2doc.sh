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

PANDOC_FILE="$1"
EXT=${2:-docx}

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS="$($SCRIPT_DIR/./pandoc_find_filters.sh)"
echo "generating docx"
cat $PANDOC_FILE | $HOME/Scripts/pandoc/preprocess_of_docx.py | \
    pandoc $PANDOC_FILTERS -o $PANDOC_FILE.$EXT
