#!/usr/bin/env bash

# install oh-my-bash
if [ -x "$(command -v curl)" ]; then
    curl -L https://get.oh-my.fish | fish
else
    echo "Need curl!"
fi
