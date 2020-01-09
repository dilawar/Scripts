#!/usr/bin/env bash
set -x -e
ADDR=${1-171.16.207.0/24}
sudo nmap -sn ${ADDR}
