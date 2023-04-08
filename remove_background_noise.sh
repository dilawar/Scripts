#!/usr/bin/env bash

FILENAME="$1"
EXT="${FILENAME##*.}"
sox $FILENAME -n trim 0 0.5 noiseprof noise.prof
sox -S --multi-threaded -buffer 131072 $FILENAME $FILENAME.fixed.$EXT noisered noise.prof 0.21 silence -l 1 0.3 5% -1 2.0 5% \;
