#!/usr/bin/env bash
set -e
git checkout main
git fetch -p && git branch -vv | awk '/: gone]/{print $1}' | xargs git branch -d
