#!/bin/bash
ghdl -a *.vhd
ghdl -m $1
ghdl -r $1 --stop-time=500ns --vcd=out.vcd
