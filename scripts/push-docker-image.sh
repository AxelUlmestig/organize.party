#!/bin/bash

set -e

IMAGE_TAG=$1

if [ -z "$IMAGE_TAG" ]; then
  echo "Please provide a tag for the image"
  exit 1
fi

sed -i -e "s/\${IMAGE_TAG:-.*}/\${IMAGE_TAG:-$IMAGE_TAG}/g" docker-compose.yml

# the Dockerfile contains the instructions for building the frontend
git clean -fxd frontend/static/ frontend/index.html
docker compose build server

docker push axelulmestig/organize.party:$IMAGE_TAG

