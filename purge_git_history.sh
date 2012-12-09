#!/bin/bash 
git filter-branch --index-filter "git rm --cached --ignore-unmatch $1" -f --prune-empty --tag-name-filte cat -- --all 
