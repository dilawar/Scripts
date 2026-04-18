#!/usr/bin/env

set -xeu
FILE="$1"

iconv -f utf8 -t ascii//TRANSLIT <"$FILE"
