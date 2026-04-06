#!/bin/bash

# Check if prod environment is up
if docker compose -p organize-party-prod -f docker-compose-prod.yml ps db | grep -q "Up"; then
    docker compose -p organize-party-prod -f docker-compose-prod.yml exec db psql \
        postgres://postgres:postgres@localhost:5432/events \
        -v PROMPT1='%[%033[1;31m%]%/%R%x%[%033[0m%]%# ' \
        -v PROMPT2='%[%033[1;31m%]%/%R%x%[%033[0m%]%# '
# Check if dev environment is up
elif docker compose ps db | grep -q "Up"; then
    docker compose exec db psql postgres://postgres:postgres@localhost:5432/events
else
    echo "Error: Neither production nor development database is running."
    exit 1
fi
