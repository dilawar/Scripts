#!/bin/bash
set -e
latexmk -bibtex -pdf "$@"
