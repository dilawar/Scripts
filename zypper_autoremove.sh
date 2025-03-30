#!/bin/sh

# Thanks https://github.com/openSUSE/zypper/issues/116#issuecomment-774486027
zypper packages --unneeded | awk 'NR>4{print $7}' | sudo xargs zypper remove --clean-deps
