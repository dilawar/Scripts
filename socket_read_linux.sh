#!/usr/bin/env sh

# read a linux socket
set -e
ncat -U "$1"
