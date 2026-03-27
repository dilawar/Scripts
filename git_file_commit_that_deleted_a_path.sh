#!/usr/bin/env bash
#
# List commit that deleted a given path.
#

set -xeu
set -o pipefail

PATH="$1"

git log --all --full-history -- "$PATH"
