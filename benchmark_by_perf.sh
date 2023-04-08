#!/usr/bin/env bash
echo "Recording using perf"
perf record --call-graph dwarf  "$@"
echo "Now showing the results."
perf report
