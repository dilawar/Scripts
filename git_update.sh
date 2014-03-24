#!/bin/bash 
set +e
echo "Updating already tracked files"
git add -u
echo "Commiting"
if [ -z "$1" ]; then
    git commit
else
    git commit -m "$1"
fi
git push origin master
