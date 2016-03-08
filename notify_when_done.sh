#!/bin/bash
set -e
(
"$@"
)
notify-send "Done doing whatever was requested by $1" 
