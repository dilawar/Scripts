#.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[000;32m\]\u@\h\[\033[00m\]:\[\033[001;34m\]\w\[\033[00m\]\$\n'
else
	PS1='${debian_chroot:+($debian_chroot)}\[\033[000;32m\]\u@\h\[\033[00m\]:\[\033[001;34m\]\w\[\033[00m\]\$\n' 
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# History support
#export HISTCONTROL=ignoredups:erasedups
#shopt -s histappend 
#export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias rm='rm -i'
alias sh='bash'
alias src='source ~/.bashrc'
alias rsync='rsync --progress'
alias i='sudo emerge -avu --autounmask-write'
alias netcat='nc.openbsd'
alias ii='sudo apt-get -c ~/.aptconf install'
alias s='apt-cache search'
alias u='sudo apt-get upgrade'
alias cpptags='ctags --c++-kinds=+p --fields=+iaS --extra=+q'
alias pandoc='pandoc --data-dir=$HOME/Scripts/pandoc'
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f $HOME/.termcap ]; then
    TERMCAP=~/.termcap
    export TERMCAP
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
export PATH=$PATH:/usr/local/bin/bin:/usr/bin:/usr/sbin:/usr/local/bin/Leo:/cad:/cad/bin:/cad/lib:/usr/local/mosml/bin
export PATH=$PATH:/sbin
export PATH=$PATH:~/Scripts/
export PATH=$PATH:~/.cabal/bin

export MMSIMHOME=/cad/cadence/mmsim
export MMSIM_PATH=/cad/cadence/mmsim/tools/bin
export MIRALIB=/cad/lib/miralib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/bin:$HOME/bin/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/Bluespec-2012.01.A/lib
#export VIM=~/Works/MyPublic/Scripts/vim
source ~/.proxy
#source ~/.bsv_sh 

export CAPEM_INSTALL=~/capem
export CAPEM_PROJECT_DIRECTORY=~/capem/projects
export PATH=$PATH:~/cad
export alias_file
export LAVA2000_Satzoo=/cad/bin/satzoo
export LAVA2000_SMV=/cad/bin/smv

# read history for each terminal
#export PROMPT_COMMAND="history -n; history -a"
export NNTPSERVER=103.21.127.130
source ~/Scripts/profile
export PATH=$PATH:~/.mutt

export PATH="$PATH:/opt/sbw-2.10.0/bin/"
export FTP_PROXY=
export TEXINPUTS=".:~/Scripts/latex/texinputs//:"
export PATH=$PATH:$HOME/bin/bin:$HOME/.local/bin
export PATH=$PATH:/usr/local/nrn/i686/bin/
export PATH=$PATH:~/Work/bin/sage-6.0-i686-Linux
export MYVIMRC=~/.vim/vimrc
export LYNX_CFG=~/Scripts/lynx.cfg
export EPREFIX=$HOME/Opt/gentoo
export HDF5_HOME=/cluster/share/software/hdf51813
export GSL_HOME=/cluster/share/software/gsl116
export PATH=$PATH:/cluster/share/software/subversion189/bin
export EDITOR=vim

source ~/Scripts/dilawar_cd.sh

# some more ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias rm='rm -i'
alias sh='bash'
alias src='source ~/.bashrc'
alias rsync='rsync --progress'
alias i='sudo emerge -avu --autounmask-write'
alias netcat='nc.openbsd'
alias ii='sudo apt-get -c ~/.aptconf install'
alias s='apt-cache search'
alias u='sudo apt-get upgrade'
alias cpptags='ctags --c++-kinds=+p --fields=+iaS --extra=+q'
alias pandoc='pandoc --data-dir=$HOME/Scripts/pandoc'
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
alias c='dilawar_cd'

export OSC_ROOT=/var/tmp/build-root/
