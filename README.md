# organize.party

## Dependencies
- docker
- docker-compose
- elm

you might need to install the following dependencies

```sh
sudo apt install -y libpq-dev zlib1g-dev postgresql postgresql-contrib
```

## Get dependencies with Nix
1. Install [Nix](https://zero-to-nix.com/start/install) and [direnv](https://github.com/nix-community/nix-direnv)
1. Add direnv hook to your shell file, e.g.
    ```sh
    echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
    ```
1. Run `direnv allow` in this repo

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

Set up daily database backups, `crontab -e` and add:
```
0 3 * * * cd /home/admin/organize.party && make backup-db
```

## Set up SSL with Let's Encrypt
1. `make deploy-production`
1. `docker compose run --rm certbot certonly --webroot --webroot-path /var/www/certbot/ -d organize.party`
1. Schedule monthly renewal of the certificate:
    `crontab -e` and add (make sure to update the path to work with your setup):
    ```
    0 0 1 * * cd /home/admin/organize.party && docker compose run --rm certbot renew
    ```

It should now be possible to view https://organize.party with full
SSL protection.


