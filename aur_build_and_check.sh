#!/usr/bin/env bash
CWD="$( pwd )"
(
    cd $CWD
    makepkg -f 
    makepkg --printsrcinfo > .SRCINFO
    namcap *.tar.xz
)

