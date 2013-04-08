#!/bin/bash
if [[ "$1" == "-d" ]]
then 
  file="$HOME/Public/Music/$(mpc -h localhost -f %file% | head -n 1)"
  rm "$file"
  echo Deleteing $file
  mpc next
fi

if [[ "$1" == "-a" ]]
then 
  file="$HOME/Pictures/Music/$(mpc -h localhost -f %file% | head -n 1)"
  mv "$file" $HOME/Public/Music/MyCollection/
  echo Adding song to collection.
fi 

