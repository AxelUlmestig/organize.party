#!/bin/bash
set -x

# docker compose down nginx
# docker compose up nginx-certbot -d

# docker compose run \
#   --rm \
#   certbot certonly \
#   --webroot \
#   --webroot-path /var/www/certbot/ \
#   -d organize.party

docker compose run \
  --rm \
  certbot certonly \
  --webroot \
  --webroot-path /var/www/certbot/ \
  --non-interactive \
  --email axel.ulmestig@gmail.com \
  --agree-tos \
  --no-eff-email \
  --domains organize.party


# docker compose down nginx-certbot
# docker compose up nginx -d

# we need to reload nginx after updating the certificate files, it's better to reload than to restart to avoid downtime. This hasn't been tested yet

docker compose exec nginx nginx -s reload
