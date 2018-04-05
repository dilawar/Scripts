#!/usr/bin/env bash

set -e
set -o nounset                                  # Treat unset variables as an error

PANDOC_FILE="$1"
EXT=${2:-html}

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PANDOC_FILTERS="$($SCRIPT_DIR/./pandoc_find_filters.sh)"
echo "generating docx"
cat $PANDOC_FILE | $HOME/Scripts/pandoc/preprocess_of_html.py | \
    pandoc $PANDOC_FILTERS \
    --standalone --self-contained --mathml \
    -o $PANDOC_FILE.$EXT
