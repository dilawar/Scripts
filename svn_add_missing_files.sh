#!/bin/bash
svn status . | awk '{if ($1 == "?") print $2}' | xargs svn add
