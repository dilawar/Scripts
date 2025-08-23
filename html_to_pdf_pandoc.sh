#!/usr/bin/env bash

set -e 
set -x

URL="https://dilawars.notion.site/resume-general"
HTMLFILE=/tmp/__1__tmp.html
curl "$URL" > "$HTMLFILE"
pandoc "$HTMLFILE" -o "$(basename ${HTMLFILE}).pdf"
