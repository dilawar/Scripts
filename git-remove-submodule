#!/bin/sh
#
# Adam Sharp
# Aug 21, 2013
#
# Usage: Add it to your PATH and `git remove-submodule path/to/submodule`.
#
# Does the inverse of `git submodule add`:
#  1) `deinit` the submodule
#  2) Remove the submodule from the index and working directory
#  3) Clean up the .gitmodules file
#

submodule_name=$(echo "$1" | sed 's/\/$//'); shift

exit_err() {
  [ $# -gt 0 ] && echo "fatal: $*" 1>&2
  exit 1
}

if git submodule status "$submodule_name" >/dev/null 2>&1; then
  git submodule deinit -f "$submodule_name"
  git rm -f "$submodule_name"

  git config -f .gitmodules --remove-section "submodule.$submodule_name"
  if [ -z "$(cat .gitmodules)" ]; then
    git rm -f .gitmodules
  else
    git add .gitmodules
  fi
else
  exit_err "Submodule '$submodule_name' not found"
fi
