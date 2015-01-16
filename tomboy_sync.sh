#!/bin/bash
TOMBOY_DIR=$HOME/Work/BITBUCKET/notes
( cd $TOMBOY_DIR && git add . && git commit -m "Syncing" && git push )
