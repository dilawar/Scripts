#!/bin/bash
# Run this script after editing any script in this directory.

if [ "$1" == "clean" ]; then
    echo "Cleaning up everything ... "
    rm -rf $HOME/.vim
    rm -rf $HOME/.mutt
    rm -rf $HOME/.config/awesome
fi

SCRIPTHOME=$HOME/Scripts

echo "Setting up Xdefaults .."
rm -f $HOME/.Xresources
ln $SCRIPTHOME/xdefaults .Xresources
ln $SCRIPTHOME/xdefaults .Xdefaults

echo "Setting up urxvt terminal..."
RXVTEXT=$HOME/.urxvt/ext
if [ ! -d $RXVTEXT ]; then
    mkdir -p $RXVTEXT 
fi 
if [ ! -f $RXVTEXT/font-size ]; then
    cd $RXVTEXT && \
    wget https://raw.github.com/majutsushi/urxvt-font-size/master/font-size \
    && cd 
fi

echo "Updating screenrc"
rm -f $HOME/.screenrc
ln $SCRIPTHOME/screenrc $HOME/.screenrc 

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

echo "Setting up mutt ..."
MUTTDIR=$HOME/.mutt
if [ -d $MUTTDIR ]; then
    cd $MUTTDIR && git pull &&  cd
else
    git clone git@github.com:dilawar:mutt $MUTTDIR
fi

echo "Updating awesome ... "
AWESOMEDIR=$HOME/.config/awesome 
if [ -d $AWESOMEDIR ]; then
    cd $AWESOMEDIR && git pull 
    echo "[INFO] Resetting awesome..."
    xdotool key Super_L+ctrl+r
else
    git clone git@github.com:dilawar/awesome $AWESOMEDIR
    echo "[TODO] Logout and login using awesome ..."
fi

echo "Setting up MPD ..."
rm -f $HOME/.mpdconf
cp $SCRIPTHOME/mpd/mpdconf $HOME/.mpdconf
MPDHOME=$HOME/.mpd
if [ -d $MPDHOME ]; then
    echo "$MPDHOME exists .. Nothing to do."
else
    mkdir $MPDHOME 
    mkdir $MPDHOME/playlists 
fi

echo "Setting up xfce4-terminal terminalrc ..."
XFCE4HOME=$HOME/.config/xfce4/terminal
if [ -d $XFCE4HOME ]; then
    rm -f $XFCE4HOME/terminalrc
    ln $SCRIPTHOME/terminalrc $XFCE4HOME/terminalrc
fi


echo "Updating vim ..."
VIMDIR=$HOME/.vim
if [ -d $VIMDIR ]; then 
    cd $VIMDIR && git pull && git submodule init && git submodule update && cd
    rm -f $HOME/.vimrc
    ln $VIMDIR/vimrc $HOME/.vimrc
else 
    git clone -b pathogen git@github.com:dilawar/vim $VIMDIR
    cd $VIMDIR && git submodule init && git submodule update && cd
fi
echo "[TODO] Open vim and run BundleInstall etc."
