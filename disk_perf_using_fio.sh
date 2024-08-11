#!/bin/bash

fio --name=test --ioengine=sync --rw=randwrite --bs=4k --numjobs=1 --size=1G --runtime=30s --time_based
