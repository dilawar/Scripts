#!/usr/bin/env bash
echo "From here https://stackoverflow.com/a/32560652/1805129"
git rev-list HEAD | xargs -n1 git ls-tree -rl | sed -e 's/[^ ]* [^ ]* \(.*\)\t.*/\1/' | sort -u | awk '{ sum += $2 } END { print sum }'
