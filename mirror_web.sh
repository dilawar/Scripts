#!/bin/bash
set -x
echo "Mirroring $@"
wget --no-clobber --mirror --no-parent --convert-links --adjust-extension \
    "$@"
