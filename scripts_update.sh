#!/bin/bash
cd $HOME/Scripts && git add -u && \
    git commit -m "Auto : $1" && git push && cd
cd $HOME/.vim && git add -u && \
    git commit -m "Auto: $1" && git push && cd

