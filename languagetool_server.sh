#!/usr/bin/env bash

set -e 
set -x


cd ~/App/LanguageTool-6.6-stable/
java -cp languagetool-server.jar org.languagetool.server.HTTPServer \
    --config server.properties \
    --port 7777 \
    --allow-origin
