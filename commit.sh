#!/bin/bash
find . -type f -regex ".*\.\(sh\|pandoc\|py\|Make*\|lua\|config\|hs\|pl\|vhd\|v\|vim\)" -print0 | xargs -0 -I file git add file 
git commit -m "$1"
git push 
