#!/bin/bash
moose_url=https://dilawars@svn.code.sf.net/p/moose/code
async_url=https://dilawars@svn.code.sf.net/p/moose/code/moose/trunk
alpha=https://dilawars@svn.code.sf.net/p/moose/code/moose/pre3.0.0

if [[ $1 = "git" ]]; then
    if [ ! $2 ]; then
        echo "Cloning with all history..."
        git svn clone $moose_url moose_git
    else
        echo "Cloning from revision $2" 
        git svn clone -s -r$2:HEAD $moose_url moose_git_shallow
    fi
elif [[ $1 = "alpha" ]]; then
    echo "git svn clone $alpha"
    git svn clone $alpha moose_3.0.0_git_svn
elif [[ $1 = "async" ]]; then
    echo "git svn clone  $async_url"
    git svn clone $async_url moose_async_git_svn
else
    svn co --username=dilawars $moose_url $1
fi

