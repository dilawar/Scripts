#!/bin/bash
MODELINFO="LaserJet Pro MFP m126nw"
LPINFO=`lpinfo  --make-and-model "$MODELINFO" -m`
echo $LPINFO
