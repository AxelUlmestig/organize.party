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
`infra/` holds an OpenTofu stack that runs the whole setup on Fogpipe Cloud:
the webapi, the worker, a managed Postgres and a bucket for photo uploads. The
platform terminates TLS, backs the database up and pools its connections.
`tofu` and `fpcloud` are in the nix dev shell; `docker` and `make` come from
the host, as they do for local development.

1. `cp infra/secrets.auto.tfvars.example infra/secrets.auto.tfvars` and fill in
   the SMTP relay. Mail is the one thing with no working default — without a
   real relay the worker starts and every send fails at the TLS handshake.
1. `fpcloud login`
1. `make deploy`

The organization is read from your login. If you belong to more than one, it
lists them and you pick: `make deploy ORG=<org-id>`. That id is the opaque one,
not the readable name — it is what image paths are built from. `TAG=` overrides
the image tag, which defaults to the current commit.

The site is served on the hostname the platform assigns. Add `host = "…"` to
your tfvars to put it on a domain of your own. The certificate is the
platform's, and the database backups are on already, so there is nothing to
schedule.

A domain you own has to be proved and pointed before it is served, so the first
apply leaves it pending. `fpcloud domain status <host>` prints the exact
records to add — a TXT proving the domain is yours, and a CNAME or A pointing
it here — and the certificate is issued once both resolve. The apply also
outputs `domain_verification` with the TXT value and where the certificate has
got to.

A hostname in the platform's own zone skips all of this: it already points here
and has no outside owner to prove.

`make deploy` is `project`, `images` and `apply` in that order. The order
matters: the registry refuses a push to a repository path no project owns yet,
and the apps cannot be created before their images exist. Any of the three runs
on its own.

The migrations are not a step here. They are the webapi's release command, so
the platform runs them itself on every deploy — in the cluster, from the image
being deployed, before that image serves anything. A migration that fails fails
the deploy and the previous version keeps serving. Nothing connects to the
database from your machine: it is cluster-internal, and `sqitch` ships inside
the image instead.

## Self-host
`make deploy-production` runs the whole stack on your local machine from
`docker-compose-prod.yml`, with the latest images pushed to docker hub
(`make push-docker-images`).

Set up daily database backups
```
make schedule-backup
```

A dump is restored with `./scripts/restore-backup.sh db_dumps/<date>.dump`.

### Set up SSL with Let's Encrypt
1. `make deploy-production`
1. `docker compose run --rm certbot certonly --webroot --webroot-path /var/www/certbot/ -d organize.party`
1. Schedule monthly renewal of the certificate:
    `crontab -e` and add (make sure to update the path to work with your setup):
    ```
    0 0 1 * * cd /home/admin/organize.party && docker compose run --rm certbot renew && docker compose restart nginx
    ```

It should now be possible to view https://organize.party with full
SSL protection.
