#!/bin/bash

set -e

IMAGE_TAG=$1

if [ -z "$IMAGE_TAG" ]; then
  echo "Please provide a tag for the image"
  exit 1
fi

git clean -fxd frontend/static/
# ./scripts/build-frontend.sh --optimize
docker compose build server

docker image tag axelulmestig/organize.party:latest axelulmestig/organize.party:$IMAGE_TAG
docker push axelulmestig/organize.party:$IMAGE_TAG

