#!/bin/bash
# Download all packages.
set -e
REPOS="Debian_7.0 Debian_8.0 Fedora_22 Fedora_20 openSUSE_13.2\
    openSUSE_Leap_42.1 CentOS_7 RHEL_7 ScientificLinux_7"
ARCHS="x86_64 i585"
for r in $REPOS; do
    for arch in $ARCHS; do
        echo "Downloading $r, $arch"
        osc getbinaries home:dilawar/moose/$r/$arch
    done
done
