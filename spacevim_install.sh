#!/usr/bin/env bash
curl -sLf https://spacevim.org/install.sh | bash
if [ ! -d $HOME/.SpaceVim.d ]; then
  git clone https://github.com/dilawar/.SpaceVim.d ~/.SpaceVim.d 
else
  cd $HOME/.SpaceVim.d/ && git pull 
fi
