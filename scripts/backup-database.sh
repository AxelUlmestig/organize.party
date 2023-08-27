#!/bin/bash
set -ex

# WARNING this will break if it's not run from the intended, top directory of
# the repo

TODAY=$(printf '%(%Y-%m-%d)T\n' -1)
DUMP_NAME="$TODAY".dump
DUMP_PATH=/tmp/$DUMP_NAME
DUMP_DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")/../db_dumps" ; pwd -P )

docker compose exec db pg_dump -f $DUMP_PATH -Fc postgres://postgres:postgres@localhost:5432/events
docker compose cp db:$DUMP_PATH $DUMP_DIR/$DUMP_NAME
docker compose exec db rm $DUMP_PATH

# only allow 10 backups to exist, delete the oldest files when the number
# exceeds 10
ls -d db_dumps/* | sort -r | sed -n "11,1000p" | xargs rm -v || true
