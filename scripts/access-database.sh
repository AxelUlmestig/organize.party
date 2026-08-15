#!/bin/bash

if docker compose ps db | grep -q "Up"; then
    docker compose exec db psql postgres://postgres:postgres@localhost:5432/events
else
    echo "Error: The development database is not running."
    exit 1
fi
