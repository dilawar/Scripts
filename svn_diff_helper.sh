#!/usr/bin/env bash
# diff helper for svn.
DIFF=vimdiff
LEFT=${6}
RIGHT=${7}
$DIFF $LEFT $RIGHT
