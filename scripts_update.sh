#!/bin/bash
(
    cd $HOME/Scripts
    git add *.sh *.py
    find . -type f -not -path '*/\.*' -executable -print0 | xargs -0 -I file git add file
    git add -u :/
    git commit -m "Auto : $1"
    $HOME/Scripts/,git_push
)
