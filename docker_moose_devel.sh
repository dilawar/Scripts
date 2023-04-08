#!/bin/bash
docker run --cap-add=SYS_PTRACE --security-opt seccomp=unconfined \
    -it dilawar/moose-opensuse /bin/bash
