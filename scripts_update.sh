#!/bin/bash
cd $HOME/Scripts && git add -u && \
    git commit -m "Auto : $1" && git pull && git push
