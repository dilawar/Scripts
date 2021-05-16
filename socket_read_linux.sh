#!/usr/bin/env sh

# read a linux socket
set -e
sudo nc -U "$1"
