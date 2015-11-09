#!/bin/bash

# THis script tryies to un-compress any file.
FILENAME="$1"
MIMETYPE=`file --mime-type "$FILENAME" | cut -d":" -f2`
# Covert MIMETYPE to lowercase, just in case
MIMETYPE=${MIMETYPE,,}
# remove leading whitespaces.
read -rd '' MIMETYPE <<< "$MIMETYPE"

function unarchive_rar 
{
    unrar e -r "$1"
}

function unarchive_zip
{
    unzip "$1"
}

function unarchive_7zip 
{
    7z x "$1"
}

function unarchive_gzip
{
    tar xvf "$1"
}

# process stuff
case "$MIMETYPE" in 
    "application/x-rar") 
        unarchive_rar "$FILENAME"
        ;;
    "application/zip")
        unarchive_zip "$FILENAME"
        ;;
    "application/pdf")
        #echo "ignore pdf"
        ;;
    "application/x-7z-compressed")
        unarchive_7zip "$FILENAME"
        ;;
    "application/gzip")
        unarchive_gzip "$FILENAME"
        ;;
    "application/x-tar")
        unarchive_gzip "$FILENAME"
        ;;
    "application/x-bzip")
        unarchive_gzip "$FILENAME"
        ;;
    *)
        echo "Unknown mimetype $MIMETYPE"
        ;;
esac
