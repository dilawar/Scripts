#!/usr/bin/env bash

# list out the processes using a port.
# This script does not give the exact answer but potential solutions. It uses
# three tools one after another.
#
#  - lsof
#  - netstat
#  - ss

set -e

PORT="$1"

echo "Looking for application using port $PORT"

if command -v lsof &> /dev/null
then
    sudo lsof -i :$PORT
fi

if command -v netstat &> /dev/null
then
    netstat -tulpn | grep ":$PORT"
    exit;
fi

if command -v ss &> /dev/null
then 
    sudo ss -tulp | grep ":$PORT"
    
    exit;
fi
