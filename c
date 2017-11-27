#!/bin/bash

# It can only cd to directory if it is a function.
function c_find 
{
    set -e
    if [ ! -d $1 ]; then
        _DIR=`dirname $1`
        _NAME=`basename $1`
        # -L : follow symbolic links.
        _TOCD=`find -L $_DIR -type d -name "*$_NAME*" | head -n 1`
        if [ ! -z $_TOCD ]; then
            _CD=`realpath $_TOCD`
            cd $_CD
        fi
    else
        cd $1
    fi
    set +e
}

alias c=c_find
