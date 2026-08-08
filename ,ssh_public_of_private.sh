#!/usr/bin/env bash
#
# Given a private key, prints its public counterpart.
#

set -euo pipefail

# Thanks https://stackoverflow.com/a/274662/1805129
PRIVATE_KEY_FILE="$1"
ssh-keygen -y -f "$PRIVATE_KEY_FILE"
