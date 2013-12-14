#!/bin/bash
moose_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code

if [ $1 == "git" ]; then
    if [ ! $2 ]; then
        echo "Cloning with all history..."
        git svn clone $moose_url moose_git
    else
        echo "Cloning from revision $2" 
        git svn clone -s -r$2:HEAD $moose_url moose_git_shallow
    fi
else
    svn co $moose_url $1
fi

