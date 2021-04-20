#!/usr/bin/env bash
set -e
set -x

# official method
# curl -s https://raw.githubusercontent.com/profclems/glab/trunk/scripts/install.sh | sudo sh
function build_and_install {
    (
    cd /tmp
    if [ ! -d glab ]; then
        git clone https://github.com/profclems/glab --depth 10
    fi
    cd glab && git branch -a && git pull

    # install if GOPATH is set.
    # https://stackoverflow.com/a/13864829/1805129
    if [ ! -z ${GOPATH+x} ]; then
        make install
    else
        echo "Either set GOPATH and run this script again or move the binary ";
        echo " to one of the \$PATH";
    fi
    )
}

if [ "$1" == "build" ]; then
    build_and_install
else
    (
        VERSION=1.16.0
        FILENAME=glab_${VERSION}_Linux_x86_64.tar.gz
        cd /tmp
        curl -JOL https://github.com/profclems/glab/releases/download/v$VERSION/${FILENAME}
        ls $FILENAME -ltrh
        if [ -f $FILENAME ]; then
            tar xvf $FILENAME -C .
            mv bin/glab $HOME/.local/bin
            chmod +x $HOME/.local/bin/glab   # just to be sure
            rm -f $FILENAME
        else
            echo "Could not download file"
        fi
    )
fi
