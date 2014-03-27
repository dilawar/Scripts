#!/bin/bash
moose_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code
multi_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code/moose/branches/buildQ/python/moose
async_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code/moose/branches/async_multiscale_cmake

if [[ $1 = "git" ]]; then
    if [ ! $2 ]; then
        echo "Cloning with all history..."
        git svn clone $moose_url moose_git
    else
        echo "Cloning from revision $2" 
        git svn clone -s -r$2:HEAD $moose_url moose_git_shallow
    fi
elif [[ $1 = "multiscale" ]]; then
    echo "git svn clone $multi_url"
    git svn clone $multi_url moose_multiscale_git_svn
elif [[ $1 = "async" ]]; then
    echo "git svn clone $async_url"
    git svn clone $async_url moose_async_multiscale_git_svn
else
    svn co --username=dilawar $moose_url $1
fi

