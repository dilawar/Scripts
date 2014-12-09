#!/bin/bash
moose_url=https://dilawars@svn.code.sf.net/p/moose/code
trunk=https://dilawars@svn.code.sf.net/p/moose/code/moose/trunk
alpha=https://dilawars@svn.code.sf.net/p/moose/code/moose/pre3.0.0
multiscale=https://dilawars@svn.code.sf.net/p/moose/code/moose/branches/async_multiscale_cmake
packages=https://dilawars@svn.code.sf.net/p/moose/code/packaging
branch=https://dilawars@svn.code.sf.net/p/moose/code/moose/branches

if [[ $2 = "svn" ]]; then
    echo "Using svn."
    CLONER="svn co"
else
    echo "Using git-svn"
    CLONER="git svn clone"
fi

if [[ $1 = "git" ]]; then
    echo "Cloning from $2 to HEAD"
    git svn clone -r$2:HEAD $moose_url moose_git_shallow
elif [[ $1 = "alpha" ]]; then
    echo "$CLONER $alpha"
    $CLONER clone $alpha moose_3.0.0_git_svn
elif [[ $1 = "trunk" ]]; then
    echo "$CLONER $trunk"
    $CLONER $trunk moose_trunk_git
elif [[ $1 = "multiscale" ]]; then
    echo "$CLONER  $multiscale"
    $CLONER $multiscale moose_multiscale_git
elif [[ $1 = "packaging" ]]; then
    echo "$CLONER $packages"
    $CLONER $packages moose_packaging
elif [[ $1 = "branch" ]]; then
    read -p "Which branch? " br
    echo "Cloning $branch/$br"
    $CLONER $branch/$br
else
    $CLONER --username=dilawars $moose_url $1
fi
