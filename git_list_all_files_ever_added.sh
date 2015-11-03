#!/bin/bash
# http://stackoverflow.com/questions/543346/list-all-the-files-that-ever-existed-in-a-git-repository
git log --pretty=format: --name-only --diff-filter=A | sort -u
