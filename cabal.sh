#!/bin/bash

(
  polipo -c ~/.polipo 
)

export http_proxy=http://localhost:8123
cabal $1

