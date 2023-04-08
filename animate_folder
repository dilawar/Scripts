#!/usr/bin/env bash

# Requires: ImageMagick.
#
# NOTE: Do not run any command in parallel since it is going to mess up the
# order in which files are modified. The mtime is used to sort files to make a
# proper gif. File which is modified earlier is stacked earlier in giff.
#
set -e

FOLDER=$1
TEMP=`mktemp -d`

if [ ! -d $FOLDER ]; then
    echo "Folder $FOLDER is not found or not directory";
    exit -1;
fi

# First convert all files to JPEG, if they are in ps or eps folder. Else simply
# copy into TEMPDIR.
# NOTE: Sort files according to mtime.
FILES=`find $FOLDER -type f -printf "%T+ %p\n" | sort | cut -d' ' -f2`
set +e
for f in $FILES; do
    _type=`file $f | cut -d' ' -f 2`
    fname=`basename $f`
    if [[ "$_type" =~ .*(Script|PDF).* ]]; then
        outfile="$TEMP/$fname.jpg"
        echo "Converting $f to $outfile " 
        if [ -f $file ]; then
            convert $f -flatten -quality 90 -density 400 $outfile \
                || echo "Could not convert $f"
        fi
    else
        echo "Copying $f to $TEMP"
        cp $f $TEMP/
    fi
done
set -e

# Now animate
JPEGS=`ls $TEMP/* -tr`
GIFFPATH=`basename ${FOLDER}`_animated.gif
convert $JPEGS $@ ${GIFFPATH}
echo "+ Wrote animated file to ${GIFFPATH}"
rm -rf $TEMP
