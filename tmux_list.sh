#!/bin/bash
# From here http://stackoverflow.com/questions/11333291/is-it-possible-to-find-tmux-sockets-currently-in-use
lsof -U | grep '^tmux'
