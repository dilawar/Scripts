#!/bin/bash
set -e
[ -d /tmp/osc ] || git clone --depth 10 https://github.com/openSUSE/osc.git /tmp/osc
(
    cd /tmp/osc.git
    python setup.py build
    python setup.py install --user
)

