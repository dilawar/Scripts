#.bashrc
# Source global definitions
export EDITOR=vim
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
export SCRIPTHOME=$HOME/Scripts
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias rm='rm -i'
alias sh='bash'
alias src='source ~/.bashrc'
alias rsync='rsync --progress'
alias i='sudo -E emerge -avuD '
alias netcat='nc.openbsd'
alias ii='sudo apt-get -c ~/.aptconf install'
alias ss='sudo -E emerge --search'
alias uu='sudo -E emerge -avuND @world'
alias cpptags='ctags h-c++-kinds=+p --fields=+iaS --extra=+q'
alias pandoc='pandoc --data-dir=$HOME/Scripts/pandoc'
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
alias sudo='sudo -E'
alias t='$SCRIPTHOME/todo.sh -d $SCRIPTHOME/todo.cfg'
alias note='terminal_velocity -x md ~/Work/notes'
alias pylint='pylint -E'
alias vi='vim'
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
export PATH=$PATH:~/Scripts/:$HOME/.mutt:$HOME:~/Scripts/data_over_git_ssh

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
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
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
export GOPATH=$HOME/go
if [ -f /etc/profile.d/bash-completion.sh ]; then
    source /etc/profile.d/bash-completion.sh 
fi
export SSHPASS=jH7qMYpF

export PATH="$PATH:/home_local/dilawars/sbw-2.10.0/bin/"
export PATH=$PATH:$HOME/.cabal/bin
export PYTHONPATH="$HOME/Work/GITHUB/DILAWAR/moose-core/_build/python"
export PYTHONPATH="$PYTHONPATH:$HOME/Work/GITHUB/DILAWAR/yacml"
export HOMEBREW_GITHUB_API_TOCKEN=8e08eccfe2ad9a8526ccf8992b4c68252fe390eb
export PATH=/opt/texlive/bin/x86_64-linux:$PATH
#export TEXMF=/usr/share/texmf
