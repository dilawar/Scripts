#!/bin/bash
set -o
set -e
NAME="$1"
docker run -d \
    -it \
    --mount type=bind,source="$(HOME)"/.vim,target=/root/.vim,readonly \
    --mount type=bind,source="$(HOME)"/.gitconfig,target=/root/.gitconfig,readonly \
    --mount type=bind,source="$(HOME)"/Scripts,target=/root/Scripts \
    bash
