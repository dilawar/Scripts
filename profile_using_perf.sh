#!/usr/bin/env bash
echo "Recording using perf"
perf record --callgraph dwarf  "$@"
echo "Now showing the results."
perf report
