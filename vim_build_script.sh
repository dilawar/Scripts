#!/usr/bin/env bash

(
    mkdir -p /tmp/vim_build
    cd /tmp/vim_build
    FILENAME=v8.1.0412.tar.gz
    if [ ! -f $FILENAME ]; then
        wget https://github.com/vim/vim/archive/$FILENAME
        tar xvf $FILENAME
    fi

    cd vim*
    ./configure  \
        --disable-nls  \
        --enable-cscope  \
        --enable-gui=yes \
        --enable-multibyte  \
        --enable-pythoninterp \
        --enable-rubyinterp  \
        --with-features=huge \
        --with-tlib=ncurses \
        --with-x
    make -j`nproc`
    sudo make install
)

