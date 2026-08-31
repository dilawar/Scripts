#!/usr/bin/env bash

git for-each-ref --format='%(authorname) %09 %(refname:short)' refs/heads/ refs/remotes/ | grep "$(git config user.name)"
