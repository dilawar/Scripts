#!/bin/bash 
vlib work 
vlog $1 && vsim -c -do 'run 1000ns;quit' $2
