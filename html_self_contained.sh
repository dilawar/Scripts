#!/usr/bin/env bash
# This script turn a download html page into self-contained html i.e. all images
# are turned into base64 strings. You must have pandoc installed.
# It writes to stdout.
pandoc -f html -t html --self-contained "$1"
