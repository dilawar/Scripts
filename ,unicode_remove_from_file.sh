#!/usr/bin/env bash

set -eu
FILE="$1"

iconv -f utf8 -t ascii//TRANSLIT <"$FILE"
