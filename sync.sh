#!/bin/bash
echo "Synching this computer with various clouds."
google=$HOME/GOOGLE_DRIVE
if [ -d $google ]; then
    echo "No GRIVE downloaded google-drive found at $google"
else
    ( cd $google && grive )
fi

VIM=$HOME/.vim
( cd $VIM && git pull )

echo "Syncing todo list"
$HOME/Scripts/todo.sh sync

echo "Syncing notes"
nsync

echo "Synching scripts"
( cd $HOME/Scripts && git pull )
