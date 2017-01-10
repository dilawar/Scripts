#!/bin/bash
svn st | grep ^! | awk '{print " --force "$2}' | xargs svn rm
