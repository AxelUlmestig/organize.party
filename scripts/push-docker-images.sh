#!/bin/bash

set -ex

WEBAPI_IMAGE_NAME=axelulmestig/organize.party-webapi
WORKER_IMAGE_NAME=axelulmestig/organize.party-worker

WEBAPI_VERSION=$(grep -i "^version:" op-webapi/op-webapi.cabal | awk '{print $2}')
WORKER_VERSION=$(grep -i "^version:" op-worker/op-worker.cabal | awk '{print $2}')

# Track what needs to be built
NEED_WEBAPI_BUILD=false
NEED_WORKER_BUILD=false

# Check which versions already exist
if ! curl -s -f -o /dev/null "https://hub.docker.com/v2/repositories/${WEBAPI_IMAGE_NAME}/tags/${WEBAPI_VERSION}"; then
  NEED_WEBAPI_BUILD=true
  echo "✓ WebAPI version $WEBAPI_VERSION needs to be built"
else
  echo "WebAPI version $WEBAPI_VERSION already exists, skipping"
fi

if ! curl -s -f -o /dev/null "https://hub.docker.com/v2/repositories/${WORKER_IMAGE_NAME}/tags/${WORKER_VERSION}"; then
  NEED_WORKER_BUILD=true
  echo "✓ Worker version $WORKER_VERSION needs to be built"
else
  echo "Worker version $WORKER_VERSION already exists, skipping"
fi

# Build all needed images first (without pushing)
if [ "$NEED_WEBAPI_BUILD" = true ]; then
  echo "Building WebAPI image..."
  # the Dockerfile contains the instructions for building the frontend
  git clean -fxd frontend/static/ frontend/index.html
  
  docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t $WEBAPI_IMAGE_NAME:$WEBAPI_VERSION \
    -t $WEBAPI_IMAGE_NAME:latest \
    -f op-webapi/Dockerfile \
    .

  echo "✓ WebAPI image built"
fi

if [ "$NEED_WORKER_BUILD" = true ]; then
  echo "Building Worker image..."
  docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t $WORKER_IMAGE_NAME:$WORKER_VERSION \
    -t $WORKER_IMAGE_NAME:latest \
    -f op-worker/Dockerfile \
    .

  echo "✓ Worker image built"
fi

# Push all built images and update docker-compose
if [ "$NEED_WEBAPI_BUILD" = true ]; then
  docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t $WEBAPI_IMAGE_NAME:$WEBAPI_VERSION \
    -t $WEBAPI_IMAGE_NAME:latest \
    -f op-webapi/Dockerfile \
    --push \
    .

  sed -i -e "s/\${WEBAPI_VERSION:-.*}/\${WEBAPI_VERSION:-$WEBAPI_VERSION}/g" docker-compose-prod.yml
  echo "✓ WebAPI images pushed"
fi

if [ "$NEED_WORKER_BUILD" = true ]; then
  echo "Building Worker image..."
  docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t $WORKER_IMAGE_NAME:$WORKER_VERSION \
    -t $WORKER_IMAGE_NAME:latest \
    -f op-worker/Dockerfile \
    --push \
    .

  sed -i -e "s/\${WORKER_VERSION:-.*}/\${WORKER_VERSION:-$WORKER_VERSION}/g" docker-compose-prod.yml
  echo "✓ Worker images pushed"
fi

