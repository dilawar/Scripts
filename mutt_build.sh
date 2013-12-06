#!/bin/bash
autoconf
CFLAGS=-m32 ./configure --prefix=/usr/local --with-curses --with-regex --enable-locales-fix --enable-pop --enable-imap --enable-smtp --with-sasl=/usr --enable-hcache --with-ssl
