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
`tofu`, `fpcloud` and `sqitch` are in the nix dev shell; `docker` and
`make` come from the host, as they do for local development.

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
your tfvars to put it on a domain of your own; the certificate is the
platform's, and the database backups are on already, so there is nothing to
schedule.

`make deploy` is `project`, `images`, `apply` and `migrate` in that order. The
order matters: the registry refuses a push to a repository path no project owns
yet, and the apps cannot be created before their images exist. Any of the four
runs on its own.
