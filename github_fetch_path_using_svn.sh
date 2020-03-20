#!/usr/bin/env bash

URL="$1"    # e.g. https://github.com/OpenSourceBrain/L5bPyrCellHayEtAl2011/tree/master/neuroConstruct/generatedNeuroML2
# Replace tree/master/ with trunk/
SVNRUL=$(echo $URL | sed 's/tree\/master/trunk/g')
svn co $SVNRUL
