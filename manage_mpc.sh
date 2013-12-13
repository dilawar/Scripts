#!/bin/bash
musicDir=$HOME/Bhandar/Music
if [[ $# -le 1 ]]; then
    echo "USAGE: $0 (-d | -a)"
fi

if [[ "$1" == "-d" ]]
then 
  file="$musicDir/$(mpc -h localhost -f %file% | head -n 1)"
  echo Deleteing $file
  rm -f "$file"
  if [ -f "$file" ]; then
      echo "File is not deleted successfully."
  fi
  mpc next
fi

if [[ "$1" == "-a" ]]
then 
  file="$musicDir/$(mpc -h localhost -f %file% | head -n 1)"
  mv "$file" $musicDir/MyCollection/
  echo Adding song to collection.
fi 

