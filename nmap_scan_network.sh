#!/usr/bin/env bash
set -x -e
ADDR=${1-172.16.207.0/24}
sudo nmap -sn ${ADDR}
