#!/bin/bash
if [ $# -lt 2 ]; then
  echo "Usage: $0 user@host key_path"
  exit
fi
ssh $1 'echo '`cat $2`' >> ~/.ssh/authorized_keys'
