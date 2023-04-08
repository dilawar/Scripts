#!/usr/bin/env bash

#===============================================================================
#
#          FILE: pandoc_find_filters.sh
#
#         USAGE: ./pandoc_find_filters.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 03/30/2018 06:25:32 PM
#      REVISION:  ---
#===============================================================================

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export PATH=$HOME/.cabal/bin:$PATH

PANDOC_FILTERS=""
# Order of filters are important.
if command -v pantable > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pantable"
fi
if command -v pandoc-imagine > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-imagine"
fi
if command -v pandoc-crossref > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-crossref"
fi
if command -v pandoc-citeproc > /dev/null; then
    PANDOC_FILTERS="$PANDOC_FILTERS -F pandoc-citeproc"
fi
PANDOC_FILTERS="$PANDOC_FILTERS -F $SCRIPT_DIR/pandoc/dilawar.py "

echo $PANDOC_FILTERS
