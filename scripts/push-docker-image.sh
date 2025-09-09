#!/bin/bash

set -e

IMAGE_TAG=$1
IMAGE_NAME=axelulmestig/organize.party

if [ -z "$IMAGE_TAG" ]; then
  echo "Please provide a tag for the image"
  exit 1
fi

sed -i -e "s/\${IMAGE_TAG:-.*}/\${IMAGE_TAG:-$IMAGE_TAG}/g" docker-compose.yml

# the Dockerfile contains the instructions for building the frontend
git clean -fxd frontend/static/ frontend/index.html
docker compose build server

# docker push axelulmestig/organize.party:$IMAGE_TAG
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t $IMAGE_NAME:$IMAGE_TAG \
  -t $IMAGE_NAME:latest \
  --push .

