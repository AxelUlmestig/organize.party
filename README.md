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
the webapi, the worker, a managed Postgres and a bucket for photo uploads.
`tofu` and `fpcloud` are included in the nix dev shell.

1. Build and push the images from `op-webapi/Dockerfile` and
   `op-worker/Dockerfile` to `registry.cloud.fogpipe.com/<org-id>/organizeparty/`
1. `fpcloud login`
1. `tofu -chdir=infra init`
1. `tofu -chdir=infra apply -var org=<org-id> -var image_tag=<tag>`, where
   `<org-id>` is the organization's opaque id — the one image paths are built
   from, not its readable name. The site is served on the hostname the platform
   assigns; add `-var host=<domain>` to put it on a domain of your own

1. Run the migrations through a tunnel:
   `fpcloud db connect events` and `sqitch --chdir db deploy` against the
   printed connection url

The certificate for the custom domain and the database backups are the
platform's, there is nothing to schedule.

