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
alias rsync='rsync --progress'
alias i='sudo emerge -avu --autounmask-write'
alias netcat='nc.openbsd'
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
export PATH=$PATH:~/Scripts/
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
#source ~/.bsv_sh 

export CAPEM_INSTALL=~/capem
export CAPEM_PROJECT_DIRECTORY=~/capem/projects
export PATH=$PATH:~/cad
export alias_file
export LAVA2000_Satzoo=/cad/bin/satzoo
export LAVA2000_SMV=/cad/bin/smv



# read history for each terminal
#export PROMPT_COMMAND="history -n; history -a"
export NNTPSERVER=pnews.iitb.ac.in
source ~/Scripts/cd.sh
source ~/Scripts/profile
export PATH=$PATH:~/.mutt
cd ~

export PATH="$PATH:/opt/sbw-2.10.0/bin/"
PATH=$PATH:/usr/local/texlive/2013/bin/i386-linux/
export FTP_PROXY=
export TEXINPUTS=".:~/Scripts/texinputs:"
export PATH=$PATH:~/Bhandar/anaconda/bin/
export MYVIMRC=~/.vim/vimrc
