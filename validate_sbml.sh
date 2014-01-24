#!/bin/bash
curl -F file=$1 -F output=xml -F offcheck=u http://sbml.org/validator/
