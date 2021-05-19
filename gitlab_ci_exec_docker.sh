#!/bin/sh

set -e

TARGET=${1:-build}
gitlab-runner exec docker $TARGET
