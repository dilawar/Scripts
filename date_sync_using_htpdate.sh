#!/usr/bin/env bash
set -x 
set -e
htpdate -s www.linux.org www.freebsd.org \
    -P proxy.ncbs.res.in:3128 
