#!/bin/bash -
#===============================================================================
#
#          FILE: docker_moose.sh
#
#         USAGE: ./docker_moose.sh
#
#   DESCRIPTION:  Run docker moose.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Wednesday 31 January 2018 05:28:43  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

docker pull bhallalab/moose:chamcham
#docker run -it \
#    --rm -v /tmp/.X11-unix:/tmp/.X11-unix \
#    -e DISPLAY=$DISPLAY bhallalab/moose:chamcham
docker run -it bhallalab/moose:chamcham /bin/bash
