#!/bin/bash

SCRIPT_HOME=$HOME/Scripts
source $SCRIPT_HOME/colors.sh

currentList=$(mpc -f "%file%" playlist)
mpc update
searched=$(mpc listall | grep -i -e "$1")
IFS=$'\n'
for song in $searched 
do 
    alreadyAdded="false"
    for current in $currentList 
    do
        if [ "$song" == "$current" ]; then
            colorPrint "INFO" "$song " " already added. Ignoring"
            alreadyAdded="true"
            break
        fi
    done
    
    if [ "$alreadyAdded" == "false" ]; then
        colorPrint "INFO" "$song" "Added"
        mpc add $song 
    fi
done
