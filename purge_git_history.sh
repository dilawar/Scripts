#!/bin/bash 
git filter-branch --index-filter --force 'git rm --cached --ignore-unmatch $1' \
  --prune-empty --tag-name-filte cat -- --all 
