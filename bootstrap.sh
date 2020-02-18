#!/bin/bash

# This script update my HOME folder. Run it once and HOME must have all default
# configuartion files of my faviorite tools.

# Check if internet connection is available.
if [ "$1" == "clean" ]; then
    colorPrint "WARN" "Cleaning up everything ... "
    rm -rf $HOME/.vim
    rm -rf $HOME/.mutt
    rm -rf $HOME/.config/awesome
    rm -rf $HOME/.Xresources $HOME/.Xdefaults
    colorPrint "TODO" "Run the script again to setup home."
    exit
fi


SCRIPTHOME=$HOME/Scripts
source $SCRIPTHOME/colors.sh

# colorPrint "STEP" "Adding myself to cool groups"
# sudo addgroup wheel power 
# sudo addgroup sound
# sudo gpasswd -a $USER sound 
# sudo gpasswd -a $USER power 
# sudo gpasswd -a $USER wheel

set +x
##colorPrint "STEP" "Setting up crontab"
### disable glob pattern.
##set -f
##CRONOUT=`crontab -l`
##CRONTEXT=`cat $SCRIPTHOME/crontab.txt`
##echo $CRONTEXT
##echo $CRONOUT
##if [[ "$CRONTEXT" == "$CRONOUT" ]]; then
##    colorPrint "INFO" "Crontab is already installed"
##else
##    colorPrint "INFO" "Installing crontab"
##    crontab $SCRIPTHOME/crontab.txt
##fi
#enable glob again.
set +f

# colorPrint "STEP" "Setting up TODO"
# sudo cp $SCRIPTHOME/todo_completion  /etc/bash_completion.d/todo

colorPrint "STEP" "Copying todo and notes"
git clone git@gitlab.com:dilawar/todo $HOME/Work/todo
git clone git@gitlab.com:dilawar/notes $HOME/Work/notes

colorPrint "STEP" "Updating submodules in Scripts"
cd $SCRIPTHOME && git submodule init && git submodule update && cd

##########################################################################
# Update bash 
colorPrint "STEP" "Updating bash"

LINE="source $SCRIPTHOME/bashrc" >> $HOME/.bashrc
if grep -Fxq "$LINE" $HOME/.bashrc
then
    echo "$LINE is already in your $HOME/.bashrc. Doing nothing here";
else
    echo "source $SCRIPTHOME/bashrc" >> $HOME/.bashrc
fi

###colorPrint "STEP" "Setting up ssh keys"
###gpg -d $SCRIPTHOME/_ssh.tar.gz.gpg > /tmp/_ssh.tar.gz
###tar xzvf /tmp/_ssh.tar.gz -C /
###echo "STEP" "Setting up permissions"
###chmod 700 $HOME/.ssh
###chmod 600 $HOME/.ssh/*
###chmod 644 $HOME/.ssh/*.pub
###chmod 640 $HOME/.ssh/authorized_keys
ln -s $SCRIPTHOME/ssh_config $HOME/.ssh/config
chmod 600 $HOME/.ssh/config

WGET="wget -e use_proxy=yes -e http_proxy=$http_proxy -e https_proxy=$https_proxy"
WGET="$WGET  --no-check-ertificate"

if [ ! -f $HOME/.gitconfig ]; then
    colorPrint "STEP"  "Configuring git."
    ln -s $SCRIPTHOME/gitconfig $HOME/.gitconfig
fi

if [ ! -f $HOME/.gitignore ]; then
    unlink $HOME/.gitignore
    ls -s $SCRIPTHOME/gitignore $HOME/.gitignore 
fi

colorPrint "STEP" "Configuring newsbeuter"
if [ ! -d $HOME/.newsbeuter/ ]; then
    ln -s $SCRIPTHOME/newsbeuter $HOME/.newsbeuter
fi

colorPrint "STEP" "Appending names of host to /etc/hosts file"
IFS=$,
hosts=""
for h in $hosts; do
    echo "Trying host $h"
    if [[ `grep "$h" /etc/hosts` == *"$h"* ]]; then 
        echo "$h already exists in file. Ignoring ..."
    else
        echo "Appending $h to /etc/hosts"
        echo $h | sudo tee -a /etc/hosts 
    fi
done
unset IFS

colorPrint "STEP" "Copynt pylintrc file"
rm -f $HOME/.pylintrc
ln -s $SCRIPTHOME/pylintrc $HOME/.pylintrc

# Setting up ncurses agent for gnupg
gnupgFile=$HOME/.gnupg/gpg-agent.conf
echo "pinentry-program /usr/bin/pinentry-curses" >> $gnupgFile 
    
##colorPrint "STEP" "Checking for flash plugin .."
##mozillaPlugin=$HOME/.mozilla/plugins
##flash_url="https://github.com/dilawar/MyPublic/blob/master/Flash/libflashplayer.so"
##
##if [ ! -d $mozillaPlugin ]; then
##    mkdir -p $mozillaPlugin 
##fi
##if [ ! -f $mozillaPlugin/libflashplayer.so ]; then
##    cd $mozillaPlugin
##    #$WGET $flash_url
##    cd 
##fi
    
## Setting up bfg
#if [ ! -f /usr/local/bin/bfg ]; then
#    colorPrint "STEP" "Download bfg to clean git repo"
#    $WGET http://repo1.maven.org/maven2/com/madgag/bfg/1.11.1/bfg-1.11.1.jar -O /tmp/bfg
#    sudo ls -s /tmp/bfg /usr/local/bin/
#    sudo chmod + /usr/local/bin/bfg
#fi


colorPrint "STEP" "Setting up Xdefaults"
rm -f $HOME/.Xresources
rm -f $HOME/.Xdefaults
ln $SCRIPTHOME/Xresources $HOME/.Xresources
ln $SCRIPTHOME/Xresources $HOME/.Xdefaults
xrdb $HOME/.Xresources

colorPrint "STEP" "Setting mailcap"
rm -f $HOME/.mailcap
ln $SCRIPTHOME/mailcap $HOME/.mailcap

colorPrint "STEP" "Setting up urxvt terminal..."
RXVTEXT=$HOME/.urxvt/ext
if [ ! -d $RXVTEXT ]; then
    mkdir -p $RXVTEXT 
fi 
rsync -azv --progress  $SCRIPTHOME/rxvt/ $RXVTEXT/

colorPrint "STEP" "Setting up dzen and conky"
if [[ $(which conky) == *"conky"* ]]; then
    rm -f $HOME/.conkyrc 
    ln $SCRIPTHOME/conkyrc $HOME/.conkyrc
else
    colorPrint "WARN" "No conky found." "Continuing..."
    ln $SCRIPTHOME/dmenu_conky $HOME/Startup/dmenu_conky
fi

colorPrint "STEP" "Updating screenrc"
rm -f $HOME/.screenrc
ln $SCRIPTHOME/screenrc $HOME/.screenrc 

colorPrint "STEP" "Setting up mercurial"
rm -f $HOME/.hgrc
ln -s $SCRIPTHOME/hgrc $HOME/.hgrc 
colorPrint "STEP" "Setting up mairix"
rm $HOME/.mairixrc
ln $SCRIPTHOME/mairixrc $HOME/.mairixrc

colorPrint "STEP" "Setting up mutt"
rm -f $HOME/.muttrc
ln $SCRIPTHOME/muttrc $HOME/.muttrc
MUTTDIR=$HOME/.mutt
if [ ! -d $HOME/.mail ]; then
    mkdir -p $HOME/.mail 
fi
if [ -d $MUTTDIR ]; then
    cd $MUTTDIR && git pull &&  cd
else
    git clone https://dilawar@gitlab.com/dilawar/mutt $MUTTDIR
    cd $MUTTDIR && git submodule init && git submodule update && cd
fi

colorPrint "STEP" "Updating awesome ... "
AWESOMEDIR=$HOME/.config/awesome 
if [ -d $AWESOMEDIR ]; then
    cd $AWESOMEDIR && git pull 
    colorPrint "INFO" "Resetting awesome"
    xdotool key Super_L+ctrl+r
else
    git clone git@github.com:dilawar/awesome $AWESOMEDIR
    cd $AWESOMEDIR && git submodule init && git submodule update && cd
    colorPrint "TODO" "Logout and login using awesome ..."
fi

colorPrint "STEP" "Setting up MPD ..."
rm -f $HOME/.mpdconf
ln -s $SCRIPTHOME/mpd/mpdconf $HOME/.mpdconf
MPDHOME=$HOME/.mpd
if [ -d $MPDHOME ]; then
    colorPrint "STEP" "$MPDHOME exists. Nothing to do."
else
    mkdir -p $MPDHOME/playlists 
    touch $MPDHOME/tag_cache 
fi

# colorPrint "STEP" "Setting up xfce4-terminal terminalrc ..."
# XFCE4HOME=$HOME/.config/xfce4/terminal
# if [ -d $XFCE4HOME ]; then
#     rm -f $XFCE4HOME/terminalrc
#     ln $SCRIPTHOME/terminalrc $XFCE4HOME/terminalrc
# fi


colorPrint "STEP" "Updating vim"
if [ ! -d $HOME/.backup ]; then
    colorPrint "STEP" " + Creating backup git dir... "
    mkdir $HOME/.backup 
    cd $HOME/.backup && git init  
fi

VIMDIR=$HOME/.vim
if [ -d $VIMDIR ]; then 
    cd $VIMDIR && git pull && git submodule init && git submodule update && cd
    rm -f $HOME/.vimrc
    ln $VIMDIR/vimrc $HOME/.vimrc
else 
    git clone git@github.com:dilawar/vim $VIMDIR
    cd $VIMDIR && git submodule init && git submodule update && cd
fi
colorPrint "TODO" "Open vim and run BundleInstall etc."

colorPrint "STEP" "Configuring awesome to be used with slim"
ln -s $SCRIPTHOME/xsession $HOME/.xsession

colorPrint "STEP" "Setting up gdb"
rm -f $HOME/.gdbinit
ln -s $SCRIPTHOME/gdbinit $HOME/.gdbinit

colorPrint "STEP" "Setting up elinks.. "
mkdir -p $HOME/.elinks
rm -f $HOME/.elinks/elinks.conf
ln $SCRIPTHOME/elinks.conf $HOME/.elinks/elinks.conf

I3HOME=$HOME/.config/i3
if [ ! -d $I3HOME ]; then
    colorPrint "STEP" "Setting up i3."
    git clone https://github.com/dilawar/i3 $I3HOME
fi

colorPrint "STEP" "Setting up latexmkrc"
rm -rf $HOME/.latekmkrc
ln $SCRIPTHOME/latexmkrc $HOME/.latexmkrc

colorPrint "STEP" "Setting up tmux"
rm -f $HOME/.tmux.conf
ln $SCRIPTHOME/tmux.conf $HOME/.tmux.conf

colorPrint "STEP" "Setting up urxvt"
git clone https://github.com/dilawar/rxvt-ext $HOME/.urxvt/ext

colorPrint "STEP" "Setting up inputrc. bash in vi mode"
ln -s $SCRIPTHOME/inputrc $HOME/.inputrc

colorPrint "STEP" "Setting up local tex paths"
MYTEX=$HOME/texmf/tex/latex/local/
mkdir -p $MYTEX 
ln -s $SCRIPTHOME/latex/poisson.* $MYTEX/

colorPrint "STEP" "Setting up HUB"
ln -s $SCRIPTHOME/hub $HOME/.config/hub

colorPrint "STEP" "Setting up matplotlibrc"
mkdir -p $HOME/.config/matplotlib/
ln -s $SCRIPTHOME/matplotlibrc $HOME/.config/matplotlib/matplotlibrc

colorPrint "STEP" "Setting up ctags"
ln -s $SCRIPTHOME/ctags $HOME/.ctags

colorPrint "STE" "Setting up lbdb"
mkdir -p $HOME/.lbdb
ln -s SCRIPTHOME/lbdb.rc $HOME/.lbdb/
