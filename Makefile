.PHONY: start-dev-backend
start-dev-backend:
	docker compose up -d db
	DB_HOST=localhost DB_PORT=5433 SMTP_SERVER=localhost SMTP_PORT=1111 SMTP_LOGIN=user SMTP_PASSWORD=password cabal run

.PHONY: build-frontend
build-frontend:
	./scripts/build-frontend.sh

.PHONY: access-database
access-database:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/events

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir db deploy

.PHONY: update-server-container
update-server-container:
	./scripts/build-frontend.sh --optimize
	docker compose up --force-recreate --build -d server
	docker image prune -f
	docker compose exec db sqitch --chdir db deploy
