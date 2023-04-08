#!/usr/bin/env bash

set -u -x -e

NAME="$1"
docker run --net host --privileged \
    --mount type=bind,source="${HOME}"/.vim,target=/root/.vim \
    --mount type=bind,source="${HOME}"/.gitconfig,target=/root/.gitconfig,readonly \
    --mount type=bind,source="${HOME}"/Scripts,target=/root/Scripts \
    -e DISPLAY=$DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -v ~/.Xauthority:/root/.Xauthority \
    -it ${NAME} bash

