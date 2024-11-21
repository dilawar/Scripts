#!/bin/bash

# Thanks https://webhostinggeeks.com/howto/use-fio-measure-speed-data-reads-writes-linux/

fio --name=test --ioengine=sync --rw=randwrite --bs=4k --numjobs=1 --size=1G --runtime=30s --time_based
