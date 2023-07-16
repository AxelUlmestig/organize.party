# organize.party

### Dependencies
- docker
- docker-compose
- elm

you might need to install the following dependencies

```sh
sudo apt install -y libpq-dev zlib1g-dev postgresql postgresql-contrib libpq-dev
```

## Run Locally
1. Create env file `cp .env.example .env` add swap values as needed
1. Build frontend `cd frontend && elm make src/Main.elm`
1. Run composed `cd - && docker-compose up`
1. Run migrations `docker-compose exec db sqitch --chdir db deploy`
1. Go to http://localhost:8081
