#.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi



# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
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
alias cpptags='ctags --c++-kinds=+p --fields=+iaS --extra=+q'
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
export PATH=$PATH:/home/dilawar/Scripts/
export PATH=$PATH:/opt/Xilinx/13.4/ISE_DS/ISE/bin/lin
export PATH=$PATH:/opt/Bluespec-2012.01.A/bin/
export PATH=$PATH:/opt/altera10.1/modelsim_ase/bin/
export PATH=$PATH:/opt/altera10.1/quartus/bin
export PATH=$PATH:/usr/local/bin/gephi/bin/

export MMSIMHOME=/cad/cadence/mmsim
export MMSIM_PATH=/cad/cadence/mmsim/tools/bin
export MIRALIB=/cad/lib/miralib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/Bluespec-2012.01.A/lib
#export VIM=~/Works/MyPublic/Scripts/vim

source ~/.proxy
source ~/.bsv_sh 

export CAPEM_INSTALL=~/capem
export CAPEM_PROJECT_DIRECTORY=~/capem/projects
export PATH=$PATH:~/cad
export alias_file
export LAVA2000_Satzoo=/cad/bin/satzoo
export LAVA2000_SMV=/cad/bin/smv


xrdb ~/Scripts/Xdefaults

# read history for each terminal
#export PROMPT_COMMAND="history -n; history -a"
export NNTPSERVER=news.iitb.ac.in
source ~/Scripts/cd.sh
alias cd='c'
source ~/Scripts/profile
