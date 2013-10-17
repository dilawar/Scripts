#!/bin/bash 
(cd notes && git pull)
hnb ~/notes/notes.hnb.xml
(cd notes && git add *.xml && git commit -m "updated" && git push)

