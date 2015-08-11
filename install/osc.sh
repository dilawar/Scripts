#!/bin/bash
set -e
git clone --depth 10 https://github.com/openSUSE/osc.git /tmp/osc.git
(
    cd /tmp/osc.git
    python setup.py install --user
)

