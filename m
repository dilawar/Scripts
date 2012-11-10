#!/bin/bash 
pwds=`gpg -d ~/.mutt/password.gpg`
eval "$pwds"
exec mutt -F ~/Scripts/mutt_server "$@"
