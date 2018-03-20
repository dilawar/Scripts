#!/bin/bash
mysqladmin -u $1 -p extended-status | \
    grep -wi 'threads_connected\|threads_running' | awk '{ print $2,$4}'
