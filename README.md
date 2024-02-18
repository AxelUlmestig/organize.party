# organize.party

## Dependencies
- docker
- docker-compose
- elm

you might need to install the following dependencies

```sh
sudo apt install -y libpq-dev zlib1g-dev postgresql postgresql-contrib libpq-dev
```

## Run Locally
1. Create env file `cp .env.example .env` add swap values as needed
1. `make build-fontend`
1. `docker compose up -d db`
1. `docker compose exec db sqitch --chdir db deploy`
1. `make start-dev-backend`
1. Go to http://localhost:8081

## Run in production
`make deploy-production`, this will run it on your local machine with the
latest pushed image from dockerhub.

## Set up SSL with Let's Encrypt
1. edit `nginx/nginx.conf` and comment out the `server` section that listens on port 443
1. `make deploy-production`
1. `docker compose run --rm certbot certonly --webroot --webroot-path /var/www/certbot/ -d organize.party`
1. restore `nginx/nginx.conf`
1. `docker compose up --force-recreate nginx`

It should now be possible to view https://organize.party with full
SSL protection.
