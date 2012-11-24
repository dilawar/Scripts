#!/bin/bash
if [[ "$1" == "-d" ]]
then 
  file="$HOME/Music/$(mpc -f %file% | head -n 1)"
  rm "$file"
  echo Deleteing $file
  mpc next
fi
