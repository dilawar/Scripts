#!/bin/bash
set -e
LIBRE=`which libreoffice` || echo "Libreoffice not found"; exit 1
$LIBRE --headless --invisible --convert-to pdf "$@"
