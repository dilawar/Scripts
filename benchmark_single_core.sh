#!/bin/bash -
#===============================================================================
#
#          FILE: benchmark_single_core.sh
#
#         USAGE: ./benchmark_single_core.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Saturday 31 March 2018 10:03:27  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
set -x -e

# This is from https://gist.github.com/joakimk/3965517
echo 'int main() { double i=0;for(i=0;i<5000000000;i++) {20%7 * i; }}' > \
    /tmp/test.c && gcc -O3 /tmp/test.c -o /tmp/test \
    && time /tmp/test && rm /tmp/test.c && rm /tmp/test

echo "[INFO] Running stress-ng"
stress-ng --cpu 4 --cpu-method matrixprod  --metrics-brief --perf -t 60

echo "[INFO] Execute phoronix test suite by yourself"
#if [ ! -d phoronix-test-suite ]; then
#    git clone --depth 10 https://github.com/phoronix-test-suite/phoronix-test-suite
#fi
#(
#    cd ./phoronix-test-suite
#    sudo bash -c ./install-sh
#)

# phoronix-test-suite batch-setup
