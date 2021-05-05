#!/usr/bin/env bash
wget -O - https://raw.githubusercontent.com/laurent22/joplin/dev/Joplin_install_and_update.sh | bash
NPM_CONFIG_PREFIX=~/.local npm install -g joplin
