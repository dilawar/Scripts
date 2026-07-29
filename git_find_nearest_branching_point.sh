#!/usr/bin/env bash

# 1. Automatically find the closest parent branch name
PARENT_BRANCH=$(git show-branch -a 2>/dev/null | grep '\*' | grep -v "$(git rev-parse --abbrev-ref HEAD)" | head -n1 | sed 's/.*\[\(.*\)\].*/\1/' | sed 's/[\~^].*//')

# 2. Automatically find the branching point SHA
BRANCHING_POINT=$(git merge-base HEAD "$PARENT_BRANCH")

echo "Parent Branch: $PARENT_BRANCH"
echo "Branching Point: $BRANCHING_POINT"
