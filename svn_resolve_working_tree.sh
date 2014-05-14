#!/usr/bin/env bash

cmd="svn resolve --accept working -R ."
echo "Executing $cmd"
eval $cmd
