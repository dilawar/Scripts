#!/bin/bash
set -x
set -e

# This version fails on special characters in filename.
# svn st | grep ^! | awk '{print " --force "$2}' | xargs svn rm
svn st | grep ! | cut -d! -f2 | sed 's/^ *//' \
    | sed 's/^/"/g' | sed 's/$/"/g' | xargs -I file svn rm --force file
