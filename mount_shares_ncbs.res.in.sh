#!/bin/bash
set -e
echo "Mouting shares.ncbs.res.in"
mkdir -p $HOME/SHARES
mount -t cifs //storage.ncbs.res.in/dilawars \
    -orw,username=dilawars,uid=50066,gid=50009,credentials=$HOME/.smbcredentials \
    $HOME/SHARES
