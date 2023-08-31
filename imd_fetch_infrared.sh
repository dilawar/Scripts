#!/bin/bash

( 
    cd $TEMP
    curl -O  https://mausam.imd.gov.in/Satellite/3Dasiasec_ctbt.jpg
    echo "Saved 3Dasiasec_ctbt.jpg to $TEMP folder"
)
