#!/bin/bash

SCRIPT_HOME=$HOME/Scripts
source $SCRIPT_HOME/colors.sh

currentList=$(mpc -f "[%position%]-[%file%]" playlist)
demanded="$@"
colorPrint "INFO" "Searching for" $demanded
IFS=$'\n'
shopt -s nocasematch
for current in "$currentList"
do
    colorPrint "[INFO]" "$current"
done
