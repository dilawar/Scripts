#!/usr/bin/env bash

set -e 
set -x
sudo lsof -i ":$1"
