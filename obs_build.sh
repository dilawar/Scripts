#!/bin/bash
# Build packaing using osc.
set -x
set -e
echo "Building $1 for repo $2"
osc build --noservice $2 --no-verify "$1" | tee __build_$1.log
