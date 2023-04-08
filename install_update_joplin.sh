#!/bin/bash

# Install or update joplin.

set -x
set -e

wget -O - https://raw.githubusercontent.com/laurent22/joplin/master/Joplin_install_and_update.sh | bash
echo "Now terminal client"
NPM_CONFIG_PREFIX=~/.joplin-bin npm install -g joplin

JOPLIN_BIN=$HOME/.local/bin/joplin

if [ -f $JOPLIN_BIN ]; then
    unlink $JOPLIN_BIN
    ln -s ~/.joplin-bin/bin/joplin $HOME/.local/bin/joplin
fi
