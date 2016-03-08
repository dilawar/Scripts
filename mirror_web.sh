#!/bin/bash
set -x
echo "Mirroring $@"
wget -r --no-clobber --mirror --no-parent --convert-links --adjust-extension \
    "$@"
