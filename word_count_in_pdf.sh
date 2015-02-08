#!/usr/bin/env bash
PDFILE=$1
pdftotext $PDFILE - | tr -d '.' | wc -w
