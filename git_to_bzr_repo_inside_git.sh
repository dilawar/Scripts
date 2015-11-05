#!/bin/bash
echo "You must be inside a git repository"
REPONAME=_git_to_bzr_repo
mkdir -p $REPONAME
git fast-export --all | (cd $REPONAME; bzr fast-import -)

