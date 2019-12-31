#!/usr/bin/env bash
docker run --rm -it \
  --user="$(id -u):$(id mu)" \
  -v "$(pwd)":/home \
  adnrv/texlive \
  lualatex --shell-escape "$@"
