#!/usr/bin/env bash
set -u
set -x
set -e
sudo dd if=$1 of=$2 conv=noerror,sync bs=4M status=progress
