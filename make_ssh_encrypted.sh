#!/bin/bash
echo "GPGing ssh keys"
tar zvcf - $HOME/.ssh | gpg -c _ssh.tar.gz.gpg
