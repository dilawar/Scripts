#!/usr/bin/env bash

JARFILE=$HOME/Scripts/textidote.jar

if [ ! -f $JARFILE ]; then
    echo "Downloading required jar file."
    wget -O $JARFILE https://github.com/sylvainhalle/textidote/releases/download/v0.6/textidote.jar 
fi
java -jar $JARFILE "$@"
