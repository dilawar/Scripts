#!/bin/bash
moose_url=svn+ssh://dilawars@svn.code.sf.net/p/moose/code

if [ $1 == "git" ]; then
    git svn clone $moose_url moose_git
else
    svn clone $moose_url moose_svn 
fi

