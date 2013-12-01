#!/bin/bash
currentList=$(mpc -f %file% playlist)
searched=$(mpc search any $1)
IFS=$'\n'
for song in $searched 
do 
    alreadyAdded="false"
    for current in $currentList 
    do
        if [ "$song" == "$current" ]; then
            echo "$song Already added. Ignoring"
            alreadyAdded="true"
            break
        fi
    done
    
    if [ "$alreadyAdded" == "false" ]; then
        echo "+ Adding $song"
        mpc add $song 
    fi
done
