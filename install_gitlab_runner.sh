#!/usr/bin/env bash
set -x -e
# See more here: https://docs.gitlab.com/runner/install/linux-manually.html
#
curl -L --output $HOME/.local/bin/gitlab-runner \
    "https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-amd64"
chmod +x $HOME/.local/bin/gitlab-runner
