#!/usr/bin/env bash

set -ex

docker stop $(docker ps -a -q)
