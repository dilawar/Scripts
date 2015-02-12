#!/bin/bash
echo "Launching in evince in quiet mode"
evince "$@" &> /dev/null &
