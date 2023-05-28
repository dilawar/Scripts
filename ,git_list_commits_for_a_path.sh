#!/bin/sh

set -n 
set -x 
set -e

FILENAME=$1
git log --all --first-parent --remotes --reflog --author-date-order -- $FILENAME
