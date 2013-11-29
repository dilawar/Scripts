#!/bin/bash
# Run this script after editing any script in this directory.

SCRIPTHOME=$HOME/Scripts
# Update bash 
echo "Updating bash"
rm -f $HOME/.bashrc
cp $SCRIPTHOME/bashrc $HOME/.bashrc 
source $HOME/.bashrc 

echo "Setting up git"
rm -f $HOME/.gitconfig 
cp $SCRIPTHOME/gitconfig $HOME/.gitconfig
rm -f $HOME/.gitignore
cp $SCRIPTHOME/gitignore $HOME/.gitignore 

echo "Setting up mutt"
MUTTDIR=$HOME/.mutt
if [ -d $MUTTDIR ]; then
    cd $MUTTDIR && git pull &&  cd
else
    git clone git@github.com:dilawar:mutt $MUTTDIR
fi

echo "Updating vim ..."
VIMDIR=$HOME/.vim
if [ -d $VIMDIR ]; then 
    (
        cd $VIMDIR && git pull && git submodule init && git submodule update 
        rm -f $HOME/.vimrc
        cp $VIMDIR/vimrc $HOME/.vimrc
    )
else 
    (
        git clone -b pathogen git@github.com:dilawar/vim $VIMDIR
        cd $VIMDIR && git submodule init && git submodule update 
    )
fi


