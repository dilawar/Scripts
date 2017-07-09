#!/bin/bash 
(
  polipo -c ~/.polipo &
)
export http_proxy=http://localhost:8123
export https_proxy=http://localhost:8123
export ftp_proxy=http://localhost:8123
easy_install "$@"
pkill polipo
