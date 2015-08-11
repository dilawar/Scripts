#!/bin/bash
set -e
git clone https://github.com/openSUSE/osc.git /tmp
(
    cd /tmp/osc.git
    python setup.py install --user
)

