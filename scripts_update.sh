#!/bin/bash
cd $HOME/Scripts && git add -u && \
    git commit -m "Auto update" && git pull && git push
