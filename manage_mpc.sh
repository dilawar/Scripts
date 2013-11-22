#!/bin/bash
musicDir=$HOME/Music
if [[ "$1" == "-d" ]]
then 
  file="$musicDir/$(mpc -h localhost -f %file% | head -n 1)"
  rm -f "$file"
  echo Deleteing $file
  mpc next
fi

if [[ "$1" == "-a" ]]
then 
  file="$musicDir/$(mpc -h localhost -f %file% | head -n 1)"
  mv "$file" $musicDir/MyCollection/
  echo Adding song to collection.
fi 

