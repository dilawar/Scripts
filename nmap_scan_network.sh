#!/usr/bin/env bash
set -x -e
ADDR=${1-171.16.1.0/24}
sudo nmap -sn ${ADDR}
