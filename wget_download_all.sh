#!/usr/bin/env bash

# First argument is type(s) of file one wants to download.
# Second argument is url.
# wget -A$1 -m -p -E -k -K -np  -e -robots=off $2
wget --no-clobber --convert-links --random-wait -r -p -E \
    -e robots=off -U mozilla \
    -A $1 \
    $2
