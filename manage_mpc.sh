#!/bin/bash
musicDir=$HOME/Bhandar/Music
collectionDir=$musicDir/MyCollection

if [[ $# -le 2 ]]; then
    echo "USAGE: $0 (-d | -a)"
fi

if [[ "$1" == "-d" ]]; then 
    file="$musicDir/$(mpc -h localhost -f %file% | head -n 1)"
    echo Deleteing $file
    rm -f "$file"
    if [ -f "$file" ]; then
        xmessage -timeout 1 "File is not deleted successfully."
    fi
    mpc next
fi

if [[ "$1" == "-a" ]]; then 
    thisFile=`mpc -h localhost -f %file% | head -n 1`
    targetFile=$(basename "$thisFile")
    file="$musicDir/$thisFile"
    cp "$file" "$collectionDir/$targetFile"
    if [ ! -f "$collectionDir/$targetFile" ]; then
        xmessage -timeout 1 "Failed to copy $thisFile"
    fi
fi 

