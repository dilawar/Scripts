#!/bin/sh

set -e
set -x

docker run \
    -v $(pwd):/app \
    --workdir /app \
    -it subcom/rust_android \
    "$@"
