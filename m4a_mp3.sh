#!/bin/bash
for f in *.m4a; do 
    if [ ! -f "${f%.m4a}.mp3" ]; then
        ffmpeg -i "$f" -acodec libmp3lame -ab 256k "${f%.m4a}.mp3"; 
    else
        echo "$f is already converted."
    fi
done
