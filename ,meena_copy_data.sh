#!/bin/bash
rsync -azv --exclude "*" --include "*.wav" \
    meena.subcom.tech:/srv/www/htdocs/meena/ .
