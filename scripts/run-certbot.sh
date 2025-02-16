#!/bin/bash
set -x

docker compose down nginx
docker compose up nginx-certbot -d

docker compose run \
  --rm \
  certbot certonly \
  --webroot \
  --webroot-path /var/www/certbot/ \
  -d organize.party

docker compose up nginx -d
