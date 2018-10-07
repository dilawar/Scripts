#!/usr/bin/env bash
docker run --publish 7080:7080 \
    --rm --volume ~/.sourcegraph/config:/etc/sourcegraph \
    --volume ~/.sourcegraph/data:/var/opt/sourcegraph \
    --volume /var/run/docker.sock:/var/run/docker.sock \
    sourcegraph/server:latest
