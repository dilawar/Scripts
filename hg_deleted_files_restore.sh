#!/usr/bin/env bash
files=`hg status . | grep '^!' | sed 's/^! //'`
if [ "$files" != "" ]; then hg revert $files; fi
