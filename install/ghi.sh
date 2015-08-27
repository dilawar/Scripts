#!/bin/bash

# this scripts install all essential packages on unix system. 
# For personal use.
(
    echo "Installing ghi (github issues)"
    curl -sL https://raw.githubusercontent.com/stephencelis/ghi/master/ghi > /tmp/ghi
    chmod 755 /tmp/ghi
    sudo mv /tmp/ghi /usr/local/bin/
)
