#!/bin/sh

set -e
set -x

git branch \
    -va \
    --sort=-committerdate \
    --format='%(committerdate:short) %(refname:short)'
