#!/bin/bash 
if [ $# -lt 1 ]; then
  echo "USAGE : ./gen_python_from_schema.sh schame.xsd"
  exit
fi
schamaName="$1"
name=$(basename $schamaName)
name=${name/".xsd"/".py"}
pyxbgen -u $1 -m $name
