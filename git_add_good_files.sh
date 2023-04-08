#!/usr/bin/env bash

is_inside_git_repo() {
    git rev-parse --is-inside-work-tree >/dev/null 2>&1
}
while IFS= read -r -d '' file; do
    _dir=$(dirname $file)
    (
        cd $_dir
        filename=$(basename $file)
        is_inside_git_repo || continue
        git add $filename || echo "File $filename is not added"
    )
done < <(find . -type f \
    -regextype posix-extended \
    \( -regex ".*\.(md|pandoc|tex|py|cpp|c|cxx|h|hpp|html|css)$" \
    \) -print0)
