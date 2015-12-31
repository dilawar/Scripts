#!/bin/bash
# Download all packages.
set -x
REPOS="Debian_7.0 Debian_8.0 \
    xUbuntu_12.04 xUbuntu_14.0 xUbuntu_15.05 xUbuntu_15.10 \
    Fedora_22 Fedora_23 \
    openSUSE_13.2 openSUSE_Leap_42.1 \
    CentOS_7 RHEL_7 ScientificLinux_7"

ARCHS="x86_64 i586"
for r in $REPOS; do
    for arch in $ARCHS; do
        echo "Downloading $r, $arch"
        (
            mkdir -p $r
            cd $r
            osc getbinaries home:dilawar/moose/$r/$arch
            osc getbinaries home:moose/moose-python/$r/$arch
        )
    done
done

