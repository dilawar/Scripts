#!/usr/bin/env bash

set -u -x -e

NAME="$1"
docker run \
    --mount type=bind,source="${HOME}"/.vim,target=/root/.vim \
    --mount type=bind,source="${HOME}"/.gitconfig,target=/root/.gitconfig,readonly \
    --mount type=bind,source="${HOME}"/Scripts,target=/root/Scripts \
    -e DISPLAY=$DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -it ${NAME} bash

