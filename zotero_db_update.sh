#!/usr/bin/env bash
echo "Update Zotero DB."
(
    cd ~/Work/GITLAB/ZoteroDB
    git add -f storage/*
    git add -f zotero.bib
    git commit -m "update to zotero."
    git pull origin master
    git push origin master 
)
