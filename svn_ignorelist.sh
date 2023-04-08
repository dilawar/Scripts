#!/usr/bin/env bash
echo "Reading .svnignore to set the svn:ignore property"
svn propset svn:ignore -RF .svnignore .
