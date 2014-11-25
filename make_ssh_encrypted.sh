#!/bin/bash
echo "GPGing ssh keys"
tar zcvf - $HOME/.ssh | gpg -c >  _ssh.tar.gz.gpg
