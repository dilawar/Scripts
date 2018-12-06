#!/usr/bin/env bash
# "See https://askubuntu.com/a/1011055/39035"
lspci -nn | grep USB  | cut -d '[' -f3 | cut -d ']' -f1 \
    | sudo xargs -I@ setpci -H1 -d @ d0.l=0
