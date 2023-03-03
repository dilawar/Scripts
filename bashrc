# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
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
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac
#.bashrc
export EDITOR=vim

# History support
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000
export HISTFILESIZE=1000000
#shopt -s histappend 

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

export SCRIPTHOME=$HOME/Scripts
if [ "$(uname)" == "Darwin" ]; then
    alias ls="gls --color=auto -ltr"
else
    alias ls='ls --color=auto'
    alias ll='ls -alF -ltr'
    alias la='ls -A -ltr'
    alias l='ls -CF -ltr'
fi
alias rm='rm -i'
alias sh='bash'
alias src='source ~/.bashrc'
alias copy='rsync -azv --progress -C'
alias i='sudo -E emerge -avuD '
alias netcat='nc.openbsd'
alias ii='sudo apt-get -c ~/.aptconf install'
alias ss='sudo -E emerge --search'
alias uu='sudo -E emerge -avuND @world'
alias cpptags='ctags h-c++-kinds=+p --fields=+iaS --extra=+q'
alias pandoc='pandoc --data-dir=$HOME/Scripts/pandoc --latex-engine=lualatex'
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
alias sudo='sudo -E'
alias t='$SCRIPTHOME/todo.sh -d $SCRIPTHOME/todo.cfg'
alias note='terminal_velocity -x md ~/Work/notes'
alias pylint='pylint -E'
alias vi='vim'
alias pdflatex="pdflatex -shell-escape"
alias lualatex="lualatex -shell-escape"
#alias ghci='stack ghci'
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
export PATH=$HOME/Scripts:$PATH
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
export PATH=~/Scripts/:$HOME/.mutt:$HOME:~/Scripts/data_over_git_ssh:$PATH

# My pandoc filters.
export PATH=$PATH:~/Scripts/pandoc

if [ -f ~/.proxy ]; then
    source ~/.proxy
fi

# read history for each terminal
#export PROMPT_COMMAND="history -n; history -a"
source ~/Scripts/profile
export PATH=$PATH:~/.mutt:$HOME/.local/bin
export LYNX_CFG=~/Scripts/lynx.cfg

if [ -f ~/Scripts/dilawar_cd.sh ]; then 
    source ~/Scripts/dilawar_cd.sh
fi

if [ -f $SCRIPTHOME/notes.sh ]; then
    source $SCRIPTHOME/notes.sh
fi

# some more ls aliases
alias rm='rm -i'
alias sh='bash'
alias src='source ~/.bashrc'
alias gist='gist -c'
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
alias antlr4='java -jar /opt/antlr/antlr-4.0-complete.jar'
alias antrlworks='/opt/antlr/antlrworks2/bin/antlrworks2'
alias gcal='gcalcli --calendar="dilawar"'
alias tmux='tmux -f $SCRIPTHOME/tmux/tmux.conf'
export GOPATH=$HOME/go
if [ -f /etc/profile.d/bash-completion.sh ]; then
    source /etc/profile.d/bash-completion.sh 
fi
export SSHPASS=jH7qMYpF

export PATH="$PATH:/home_local/dilawars/sbw-2.10.0/bin/"
export PATH=$PATH:$HOME/.cabal/bin
export PYTHONPATH="$HOME/Work/GITHUB/DILAWAR/moose-core/_build/python"
#export PYTHONPATH="$HOME/Work/GITHUB/DILAWAR/ngspyce/"
#export PYTHONPATH="$PYTHONPATH:$HOME/Work/GITHUB/DILAWAR/yacml"
export HOMEBREW_GITHUB_API_TOCKEN=8e08eccfe2ad9a8526ccf8992b4c68252fe390eb
#export PATH=/opt/texlive/2016/bin/x86_64-linux/:$PATH
#export TEXMF=/usr/share/texmf

# added by travis gem
[ -f /home1/dilawars/.travis/travis.sh ] && source /home1/dilawars/.travis/travis.sh

# Open JDK.
export JAVA_HOME=/usr/lib64/jvm/java-openjdk/
alias java='java ${JAVA_FLAGS}'

# Make sure that java launches with awesome windowmanager
export AWT_TOOLKIT=MToolkit
export PYTHONSTARTUP=$SCRIPTHOME/python_startup.py


# User scripts
# source bash-preexec.sh 
# Now for very command prefix is with alert command so that I can get notified.
source alert.sh
xrdb ~/Scripts/Xresources
