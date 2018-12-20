#!/usr/bin/env bash
# This one is from https://stackoverflow.com/a/18213120/1805129 
for f in *\ *; do mv "$f" "${f// /_}"; done
