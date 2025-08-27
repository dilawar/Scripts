#!/usr/bin/env bash

set -e
set -x

docker run --rm -it -p 8010:8010 \
    -e Java_Xms=512m -e Java_Xmx=2g \
    erikvl87/languagetool &
