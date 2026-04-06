#!/bin/bash

# Wait for PostgreSQL in the container to be ready
COMPOSE_FILE=${1:-docker-compose.yml}
CONTAINER_NAME=db
MAX_ATTEMPTS=30
SLEEP_SECONDS=0.5

for i in $(seq 1 $MAX_ATTEMPTS); do
    # Check if pg_isready returns success in the container
    if docker compose -f "$COMPOSE_FILE" exec "$CONTAINER_NAME" pg_isready -U postgres >/dev/null 2>&1; then
        echo "Database is ready!"
        exit 0
    fi

    echo "Attempt $i/$MAX_ATTEMPTS: Database not ready yet. Waiting $SLEEP_SECONDS seconds..."
    sleep $SLEEP_SECONDS
done

echo "Failed to connect to database after $MAX_ATTEMPTS attempts" >&2
exit 1
