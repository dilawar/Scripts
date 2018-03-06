#!/bin/bash -
#===============================================================================
#
#          FILE: git_last_pushed_commit.sh
#
#         USAGE: ./git_last_pushed_commit.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Tuesday 06 March 2018 12:04:21  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
BRANCH=${1:-master}
#git log --pretty=oneline --abbrev-commit --graph --decorate --all
git rev-parse origin/$BRANCH
