#!/bin/bash -e
(
  polipo -c ~/.polipo &
)

export http_proxy=http://localhost:8123
if find $HOME/.cabal/packages/hackage.haskell.org -maxdepth 0 -empty | read; then 
  echo "Running cabal update"
  cabal update 
else 
  echo "Package list already exists.. Downloaing..."
fi
cabal "$@"
pkill polipo
