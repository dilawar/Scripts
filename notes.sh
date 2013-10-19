#!/bin/bash 
(
  cd $(pwd)/notes && git pull
)
hnb ~/notes/notes.hnb.xml

(
    cd notes && git add *.xml && git commit -m "updated" && git push
)

