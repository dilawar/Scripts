#!/usr/bin/env bash
sudo dd if=$1 of=$2 conv=noerror,sync bs=4M status=progress
