#!/usr/bin/env bash
pandoc -f html -t html --selfcontained "$1"
