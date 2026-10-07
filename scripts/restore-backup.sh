#!/bin/bash

DUMP_PATH=$1
COMPOSE_FILE="docker-compose-prod.yml"

if [ -z "$DUMP_PATH" ]; then
  echo "Please provide a file to back up from"
  exit 1
fi

if [ ! -f "$DUMP_PATH" ]; then
  echo "File not found"
  exit 1
fi

read -p "Type 'yes' to confirm that you want to overwrite the existing database: " -n 3 -r
echo "" # move to new line
if [[ ! $REPLY == "yes" ]]
then
  echo "$REPLY is not 'yes', exiting"
  exit 1
fi

set -ex

DUMP_FILE_NAME=/tmp/$(basename "$DUMP_PATH")

docker compose -f $COMPOSE_FILE cp $DUMP_PATH db:$DUMP_FILE_NAME
docker compose -f $COMPOSE_FILE exec db pg_restore --clean --if-exists -d postgres://postgres:postgres@localhost:5432/events $DUMP_FILE_NAME
docker compose -f $COMPOSE_FILE exec db rm $DUMP_FILE_NAME
