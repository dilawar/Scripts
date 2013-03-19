#!/bin/bash
if [[ "$1" == "-d" ]]
then 
  file="$HOME/Music/$(mpc -h localhost -f %file% | head -n 1)"
  rm "$file"
  echo Deleteing $file
  mpc next
fi

if [[ "$1" == "-a" ]]
then 
  file="$HOME/Music/$(mpc -h localhost -f %file% | head -n 1)"
  mv "$file" $HOME/Music/MyCollection/
  echo Adding song to collection.
fi 

