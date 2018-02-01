#!/bin/bash -
#===============================================================================
#
#          FILE: docker_remove_untagged_images.sh
#
#         USAGE: ./docker_remove_untagged_images.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Thursday 01 February 2018 09:59:58  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
docker rmi $(docker images | grep "^<none>" | awk '{print $3}') 
