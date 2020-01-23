#!/usr/bin/env bash
docker run -w /osc -v $(PWD):$(PWD) -it dilawars/osc \
  osc "$@"

