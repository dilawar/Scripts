#!/usr/bin/env bash

echo "It is from https://davidwalsh.name/docker-remove-all-images-containers"
# Delete every Docker containers
# Must be run first because images are attached to containers
docker rm -f $(docker ps -a -q)
# Delete every Docker image
docker rmi -f $(docker images -q)
