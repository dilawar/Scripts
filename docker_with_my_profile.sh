#!/bin/bash
set -o
set -e
NAME="$1"
docker run -d \
    -it \
    --mount type=bind,source="${HOME}"/.vim,target=/root/,readonly \
    --mount type=bind,source="${HOME}"/.gitconfig,target=/root,readonly \
    --mount type=bind,source="${HOME}"/Scripts,target=/root\
    ${NAME} bash
