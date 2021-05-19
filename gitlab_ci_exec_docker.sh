#!/bin/sh

set -e
TARGET=${1:-build}

(
    cd $(git rev-parse --show-toplevel)
    gitlab-runner exec docker $TARGET
)
