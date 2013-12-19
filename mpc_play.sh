#!/bin/bash

SCRIPT_HOME=$HOME/Scripts
source $SCRIPT_HOME/colors.sh

currentList=$(mpc -f "[%position%]-[%file%]" playlist)
demanded=$1
colorPrint "INFO" "Searching for" $1
IFS=$'\n'
shopt -s nocasematch
for current in $currentList 
do
    echo $current | grep $demanded
    if [[ $current == *"$demanded"* ]]; then
        position=${current%%-*}
        colorPrint "INFO" "A matching song is found in playlist at $position"
        mpc play $position
        exit
    fi
done
colorPrint "INFO" "No song found for your query." 
