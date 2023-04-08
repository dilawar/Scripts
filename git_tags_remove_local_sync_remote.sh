#!/usr/bin/env sh
git tag -l | xargs git tag -d
git fetch --tags
