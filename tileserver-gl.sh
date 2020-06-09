#!/bin/bash
set -x
DC_OPTS="--rm -u $(id -u):$(id -g)"
TPORT=8080
docker run $DC_OPTS -it --name tileserver-gl \
    -v $(pwd):/data \
    -p $TPORT:$TPORT \
    maptiler/tileserver-gl \
    --mbtiles "$1" --port $TPORT
