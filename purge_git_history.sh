#!/bin/bash 
git filter-branch -f --index-filter "git rm --cached --ignore-unmatch $1" --prune-empty --tag-name-filter cat -- --all 
git gc --aggressive --prune=now
