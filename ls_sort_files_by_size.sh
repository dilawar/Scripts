#!/usr/bin/env bash
find -type f -printf "%s\t%k KB\t%p\n" | sort -nr | head -20
