#!/usr/bin/env bash

N_SHOW=${1-20}
find . -type f -printf '%TY-%Tm-%Td %TT %p\n' | sort -r | head -n $N_SHOW
