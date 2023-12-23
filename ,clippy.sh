#!/usr/bin/env bash

cargo clippy -- \
    -Wclippy::restriction \
    -Wclippy::style -Wclippy::double_neg -Dclippy::perf \
    -Wclippy::missing_docs 
