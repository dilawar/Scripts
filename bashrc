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

# Thanks https://unix.stackexchange.com/a/48116/5362
HISTSIZE=9000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignorespace:ignoredups

_bash_history_sync() {
    builtin history -a         #1
    HISTFILESIZE=$HISTSIZE     #2
    builtin history -c         #3
    builtin history -r         #4
}

history() {                  #5
    _bash_history_sync
    builtin history "$@"
}

PROMPT_COMMAND=_bash_history_sync

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
alias copy='rsync -azv --progress -C'
alias cpptags='ctags --exclude=node_modules/* --exclude=vendor/*'
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
alias pylint='pylint -E'
alias pdflatex="pdflatex -shell-escape"
alias lualatex="lualatex -shell-escape"

#
# TODO
#
function t {
    source $SCRIPTHOME/todo.cfg
    if [ ! -d $TODO_DIR ]; then
        echo "Clone into $TODO_DIR"
        git clone git@gitlab.com:dilawar/todo $TODO_DIR
    fi
    $SCRIPTHOME/todo.sh -d $SCRIPTHOME/todo.cfg $@
}

# Alias for tmux.
alias tmux="/usr/bin/tmux -f /$SCRIPTHOME/tmux/tmux.conf"

# mypy
alias mypy="mypy --config /$SCRIPTHOME/mypy.ini"

# alias ghci='stack ghci'
# alias ghc='stack ghc'
# alias runghc='stack runghc'

# Create alias for vim to launch it in profile mode.
alias vimstartup="vim --startuptime /$HOME/.cache/vim_startup.log "

if [ -f "$HOME/.bash_aliases" ]; then
    source "$HOME/.bash_aliases"
fi

if [ -f "$HOME/.termcap" ]; then
    TERMCAP=~/.termcap
    export TERMCAP
fi

# mutt
export PATH=$SCRIPTHOME:$HOME/.mutt:$PATH

# android goodies
if [ -d "$HOME/Android/Sdk/platform-tools" ]; then
    export PATH=$HOME/Android/Sdk/platform-tools:$PATH
fi

# vim mode in bash
set -o vi

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
alias lynx='lynx --cfg=$HOME/Scripts/lynx.cfg'
alias antlr4='java -jar /opt/antlr/antlr-4.0-complete.jar'
alias antrlworks='/opt/antlr/antlrworks2/bin/antlrworks2'
alias gcal='gcalcli --calendar="dilawar"'
alias move='mv -v -u -n'
# Notify when done.
alias lwd='$HOME/Scripts/notify_when_done.sh'

# NNN
export NNN_USE_EDITOR=1
export NNN_DE_FILE_MANAGER=thunar

export GOPATH=$HOME/go

SSH_ENV="$HOME/.ssh/agent-environment"
function start_agent {
    # echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    # echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

# This is invalid since I no longer behind proxy.
#export JAVA_FLAGS="-Dhttp.proxyHost=proxy.ncbs.res.in -Dhttp.proxyPort=3128"
#alias java="java ${JAVA_FLAGS}"

# To make sure that java launches with awesome windowmanager
export AWT_TOOLKIT=MToolkit

if [ -f "$SCRIPTHOME/git-prompt.sh" ]; then
    source "$SCRIPTHOME/git-prompt.sh"
    export PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
fi

if [ -f /etc/profile.d/autojump.bash ]; then
    source /etc/profile.d/autojump.bash
fi

fasd_cache="$HOME/.fasd-init-bash"
$SCRIPTHOME/fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"

source "$fasd_cache"
_fasd_bash_hook_cmd_complete v m j o
unset fasd_cache

source $SCRIPTHOME/fasd
alias c='fasd_cd -d'
alias vv='f -e vim' # quick opening files with vim
alias mp='f -e mplayer' # quick opening files with mplayer
alias o='a -e xdg-open' # quick opening files with xdg-open

export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH

# HOME/.local/bin
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.nimble/bin:$PATH   # nim
export TERMINAL=xfce4-terminal

# mypy cache directory. By default, mypy create cache in the source directory.
# ctags creates tags from this file and move to cache file on tag jump which is
# so annoying.
export MYPY_CACHE_DIR=$HOME/.cache/mypy
mkdir -p $MYPY_CACHE_DIR

# ruby
export GEM_HOME=$HOME/.gem
export GEM_PATH=$HOME/.gem
export PATH=$HOME/.gem/bin:$PATH

" navigation
function rgv() { vim -c "silent grep $1" -c "copen"; }
