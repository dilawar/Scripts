#!/bin/bash

set -x 
set -e
set -n

FILENAME=$1
echo "Listing commits for $FILENAME"
git log --all --first-parent --remotes --reflog --author-date-order -- $FILENAME
