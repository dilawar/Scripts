#!/bin/bash
# Download all packages.
set -e
REPOS=`osc repourls $@`
IFS=$'\n'
for r in $REPOS; do
    url=`dirname $r`
    echo "Downloading rpm an deb"
    wget -r --no-parent --reject ".*src" -A "*.rpm,*.deb" $url
done
