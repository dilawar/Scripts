#!/usr/bin/env bash
set -u
set -x
set -e
sudo dd if=$1 of=$2 conv=sync bs=4M status=progress
sudo sync
notify-send "Done writing $1 to $2"
