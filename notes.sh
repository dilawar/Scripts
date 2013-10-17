#!/bin/bash 
(cd notes && git pull)
hnb 
(cd notes && git add . && git commit -m "updated" && git push)

