#!/bin/sh

set -xe

# Thanks https://github.com/openSUSE/zypper/issues/116#issuecomment-774486027
# and https://github.com/openSUSE/zypper/issues/116#issuecomment-1492793358
zypper packages --unneeded | awk -F'|' 'NR==0 || NR==1 || NR==2 || NR==3 || NR==4 {next} {print $3}' | grep -v Name | sudo xargs zypper remove --clean-deps
