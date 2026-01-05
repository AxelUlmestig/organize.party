.PHONY: deploy-database
deploy-database:
	docker compose up -d pgbouncer
	./scripts/wait-for-db.sh
	docker compose exec db sqitch --chdir /repo/statecharts -t postgres://postgres:postgres@localhost:5432/events deploy
	docker compose exec db sqitch --chdir db deploy

.PHONY: start-dev-webapi
start-dev-webapi: deploy-database
	HOST_URL=http://localhost:8081 DB_HOST=localhost DB_PORT=6432 cabal run op-webapi

.PHONY: start-dev-worker
start-dev-worker: deploy-database
	docker compose up -d mailhog
	LOG_LEVEL=LevelDebug DB_HOST=localhost DB_PORT=6432 LISTEN_DB_HOST=localhost LISTEN_DB_PORT=5432 SMTP_SERVER=localhost SMTP_PORT=1025 SMTP_LOGIN= SMTP_PASSWORD= cabal run op-worker

.PHONY: build-frontend
build-frontend:
	./scripts/build-frontend.sh

.PHONY: access-database
access-database:
	docker compose exec pgbouncer psql postgres://postgres:postgres@pgbouncer:6432/events

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir db deploy

.PHONY: update-server-container
update-server-container:
	./scripts/build-frontend.sh --optimize
	docker compose up --force-recreate --build -d server
	docker image prune -f
	docker compose exec db sqitch --chdir db deploy

.PHONY: deploy-production
deploy-production:
	docker compose up -d db
	./scripts/wait-for-db.sh
	docker compose exec db sqitch --chdir db deploy
	docker compose up --force-recreate -d production

.PHONY: backup-db
backup-db:
	./scripts/backup-database.sh

.PHONY: schedule-backup
schedule-backup:
	./scripts/schedule-backup.sh

.PHONY: run-certbot
run-certbot:
	./scripts/run-certbot.sh

.PHONY: run-tests
run-tests:
	cd frontend/test && npx playwright test --headed test.spec.ts --project chromium

.PHONY: lint
lint:
	hlint -X QuasiQuotes -X OverloadedRecordDot .
