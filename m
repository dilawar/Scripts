#!/bin/bash 
pwds=`gpg -d --no-use-agent  ~/Scripts/mutt/password.gpg'
eval "$pwds"
exec mutt -F ~/Scripts/mutt_server "$@"
