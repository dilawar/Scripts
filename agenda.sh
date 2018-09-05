#!/usr/bin/env bash
AGENDA=$(gcalcli --nostarted --nocolor agenda | head -2 | tail -1)
echo $AGENDA
