#!/bin/bash
mpc search any $1 | xargs -I file mpc add file
