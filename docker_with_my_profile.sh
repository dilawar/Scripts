#!/bin/bash
set -u -x -e
NAME="$1"
docker run \
    --mount type=bind,source="${HOME}"/.vim,target=/root/.vim,readonly \
    --mount type=bind,source="${HOME}"/.gitconfig,target=/root/.gitconfig,readonly \
    --mount type=bind,source="${HOME}"/Scripts,target=/root/Scripts \
    -it ${NAME} bash
