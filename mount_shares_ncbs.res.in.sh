#!/bin/bash
set -e
echo "Mouting shares.ncbs.res.in"
mount -t cifs //shares.ncbs.res.in/dilawars \
    -orw,username=dilawars,uid=50066,gid=50009,credentials=$HOME/.smbcredentials \
    /shares.ncbs.res.in/dilawars
