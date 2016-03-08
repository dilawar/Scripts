#!/bin/bash
set -e
(
"$@"
)
notify-send "Done $@" 
