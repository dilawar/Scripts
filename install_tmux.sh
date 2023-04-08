#!/usr/bin/env bash

set -e
set -x

# install tmux and byobu
VERSION=3.1b
(
    cd /tmp
    wget \
        https://github.com/nelsonenzo/tmux-appimage/releases/download/tmux$VERSION/tmux-$VERSION-x86_64.AppImage
    mv tmux-$VERSION-x86_64.AppImage $HOME/.local/bin/tmux
    chmod +x $HOME/.local/bin/tmux
)
