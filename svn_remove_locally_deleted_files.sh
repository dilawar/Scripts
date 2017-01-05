#!/bin/bash

svn st | grep ^! | awk '{print  --force }' | xargs svn rm
