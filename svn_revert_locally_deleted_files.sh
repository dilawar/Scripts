#!/bin/bash
svn status | grep ^D | awk '{print }' | xargs svn revert
