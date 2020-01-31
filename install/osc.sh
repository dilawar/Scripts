#!/bin/bash
set -e
[ -d /tmp/osc ] || git clone --depth 10 https://github.com/openSUSE/osc /tmp/osc
(
    cd /tmp/osc
    python setup.py build
    python setup.py install --user
)

