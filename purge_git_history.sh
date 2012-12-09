#!/bin/bash 
git filter-branch --index-filter 'git rm --cached --ignore-unmatch $1' \
  --prune-empty --tag-name-filter -f cat -- --all 
