#!/usr/bin/env bash
curl -sLf https://spacevim.org/install.sh | bash
if [ ! -d $HOME/.Spacevim.d ]; then
    git clone https://github.com/dilawar/.SpaceVim.d ~/.SpaceVim.d 
fi
