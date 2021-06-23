#.bashrc
export EDITOR=vim
shopt -s histappend

# some more ls aliases
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
alias t='$SCRIPTHOME/todo.sh -d $SCRIPTHOME/todo.cfg'
alias pylint='pylint -E'
alias pdflatex="pdflatex -shell-escape"
alias lualatex="lualatex -shell-escape"

# Alias for tmux.
alias tmux="tmux -f \$SCRIPTHOME/tmux/tmux.conf"

# mypy
alias mypy="mypy --config \$SCRIPTHOME/mypy.ini"

# alias ghci='stack ghci'
# alias ghc='stack ghc'
# alias runghc='stack runghc'

# Create alias for vim to launch it in profile mode.
alias vimstartup="vim --startuptime \$HOME/.cache/vim_startup.log "

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
export HOMEBREW_GITHUB_API_TOCKEN=8e08eccfe2ad9a8526ccf8992b4c68252fe390eb

# This is invalid since I no longer behind proxy.
#export JAVA_FLAGS="-Dhttp.proxyHost=proxy.ncbs.res.in -Dhttp.proxyPort=3128"
#alias java="java ${JAVA_FLAGS}"

# To make sure that java launches with awesome windowmanager
export AWT_TOOLKIT=MToolkit

if [ -f "$SCRIPTHOME/git_prompt.sh" ]; then
    source "$SCRIPTHOME/git_prompt.sh"
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
export TERMINAL=gnome-terminal

# pyenv
if [ -d $SCRIPTHOME/pyenv ]; then
    export PYENV_ROOT=$SCRIPTHOME/pyenv
    export PATH=$PYENV_ROOT/bin:$PATH
fi

if command -v pyenv 1>/dev/null 2>&1 ; then
    eval "$(pyenv init -)"
fi

# brew settings.
if type brew &>/dev/null; then
    HOMEBREW_PREFIX="$(brew --prefix)"
    if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
        source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
    else
        for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
            [[ -r "$COMPLETION" ]] && source "$COMPLETION"
        done
    fi
fi

# mypy cache directory. By default, mypy create cache in the source directory.
# ctags creates tags from this file and move to cache file on tag jump which is
# so annoying.
export MYPY_CACHE_DIR=$HOME/.cache/mypy
mkdir -p $MYPY_CACHE_DIR

# github token
alias ghtoken='echo $GITHUB_TOKEN'

# ruby
export GEM_HOME=$HOME/.gem
export GEM_PATH=$HOME/.gem
export PATH=$HOME/.gem/bin:$PATH

TLMGR_BIN="/usr/share/texmf-dist/scripts/texlive/tlmgr.pl"
if [ -f "$TLMGR_BIN" ]; then
    alias tlmgr="$TLMGR_BIN --usermode"
fi
