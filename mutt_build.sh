#!/bin/bash
mutturl=ftp://ftp.mutt.org/mutt/mutt-1.4.2.3.tar.gz
cd /tmp 
wget $mutturl 
tar xzvf mutt*.tar.gz
cd mutt*
autoconf
CFLAGS=-m32 ./configure --prefix=/usr/local --with-curses --with-regex --enable-locales-fix --enable-pop --enable-imap --enable-smtp --with-sasl --enable-hcache --with-ssl
make && sudo make install
