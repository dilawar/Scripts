#!/bin/bash 
if [ "$1" = "x" ] 
then
  echo "Pushing web on sharada to origin."
  ssh dilawar@sharada.ee.iitb.ac.in "cd public_html && git add . && git commit -m \"Remove updated \" && git push";
elif [ "$1" = "y" ] 
then 
  echo "Pulling web on sharada from origin."
  ssh dilawar@sharada.ee.iitb.ac.in "cd public_html && git pull";
else 
  echo "Usage : arg x to push or y to pull."
fi

