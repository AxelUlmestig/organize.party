#!/bin/bash

set -e

CRON_CONFIG="0 3 * * * cd $PWD && make backup-db"
EXISTING_CONFIG="$(crontab -l)"

if echo "$EXISTING_CONFIG" | grep -cFxq "$CRON_CONFIG"; then
  echo "crontab already configured"
  exit 0
fi

echo "0 3 * * * cd $PWD && make backup-db" | crontab -
echo "crontab updated"
