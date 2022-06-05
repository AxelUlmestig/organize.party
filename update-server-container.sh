#!/bin/sh

(cd frontend; elm make src/Main.elm)
docker-compose up --force-recreate --build -d server
docker image prune -f
docker-compose exec db sqitch --chdir db deploy
