#!/bin/bash 
git filter-branch --index-filter "git rm -f --cached --ignore-unmatch $1" --prune-empty --tag-name-filte cat -- --all 
