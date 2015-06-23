#!/bin/bash

if [ -z "$MUSIC_DIR" ]; then
    echo "MUSIC_DIR var is not set."
    echo "Defaulting to $HOME/Music"
    MUSIC_DIR=$HOME/Music
fi

collectionDir=$MUSIC_DIR/MyCollection
if [ ! -d $collectionDir ]; then
    mkdir -p $collectionDir 
fi

if [[ "$1" == "-d" ]]; then 
    file="$MUSIC_DIR/$(mpc -h localhost -f %file% | head -n 1)"
    echo Deleteing $file
    rm -f "$file"
    if [ -f "$file" ]; then
        xmessage -timeout 1 "File is not deleted successfully."
    fi
    mpc next
elif [[ "$1" == "-a" ]]; then 
    thisFile=`mpc -h localhost -f %file% | head -n 1`
    targetFile=$(basename "$thisFile")
    file="$MUSIC_DIR/$thisFile"
    cp "$file" "$collectionDir/$targetFile"
    if [ ! -f "$collectionDir/$targetFile" ]; then
        xmessage -timeout 1 "Failed to copy $thisFile to $collectionDir"
    fi
else
    echo "USAGE: $0 (-d | -a)"
fi
