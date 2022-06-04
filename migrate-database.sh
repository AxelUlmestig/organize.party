#!/bin/sh

docker-compose exec db sqitch --chdir db deploy
