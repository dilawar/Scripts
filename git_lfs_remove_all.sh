#!/usr/bin/env sh

# This is from here https://github.com/git-lfs/git-lfs/issues/910#issuecomment-344150888

git filter-branch -f --prune-empty --tree-filter '
if [ -f .gitattributes ]; then 
    git rm -f .gitattributes
fi

find . -type f | while read FILE; do 
while head -2 "$FILE" | grep -q "^oid sha256"; do
    ref=$(cat "$FILE")
    echo -n "$ref" | git lfs smudge > "$FILE"
    git add "$FILE"
done
done' --tag-name-filter cat -- --all
