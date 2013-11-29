#!/bin/bash
svn status | grep -v ^.[ t]*..* | grep ^? | awk '{print }' | xargs -I file svn add file 
svn commit
