#!/usr/bin/env bash

(
    mkdir -p /tmp/vim_build
    cd /tmp/vim_build
    if [ ! -d vim ]; then
        git clone https://github.com/vim/vim --depth 1
        cd vim
    else
        cd vim && git pull origin master
    fi
    ./configure  \
        --disable-nls  \
        --enable-cscope  \
        --enable-gui=yes \
        --enable-multibyte  \
        --enable-gui=auto \
        --enable-rubyinterp  \
        --enable-python3interp \
        --enable-cscope \
        --with-features=huge \
        --with-tlib=ncurses \
        --prefix=$HOME/.local \
        --with-x
    make -j`nproc`
    make install
)

