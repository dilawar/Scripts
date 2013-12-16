#!/bin/bash
moose_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code
multi_url=svn://svn.code.sf.net/p/moose/code/moose/branches/buildQ/python/moose

if [ $1 == "git" ]; then
    if [ ! $2 ]; then
        echo "Cloning with all history..."
        git svn clone $moose_url moose_git
    else
        echo "Cloning from revision $2" 
        git svn clone -s -r$2:HEAD $moose_url moose_git_shallow
    fi
elif [ $1 == "multiscale" ]; then
    echo "git svn clone $multi_url"
    git svn clone $multi_url moose_multiscale_git_svn
else
    svn co $moose_url $1
fi

