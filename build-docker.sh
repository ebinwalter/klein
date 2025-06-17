#!/bin/sh

DOCKER_TAG=${1:-klein}
DOCKER_DEFAULT_PLATFORM=${2:-linux/amd64}

docker build --rm --platform $DOCKER_DEFAULT_PLATFORM -t $DOCKER_TAG .
