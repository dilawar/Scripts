#!/bin/bash -
#===============================================================================
#
#          FILE: youtube_download_playlist_as_audio.sh
#
#         USAGE: ./youtube_download_playlist_as_audio.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Friday 09 March 2018 09:21:55  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
echo "Downloading: $1"
youtube-dl --output "%(title)s.%(ext)s" --extract-audio --audio-format mp3 \
    --audio-quality 0  "$@"
