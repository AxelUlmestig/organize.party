.PHONY: start-dev-backend
start-dev-backend:
	docker compose up -d pgbouncer mailhog
	./scripts/wait-for-db.sh
	docker compose exec db sqitch --chdir db deploy
	HOST_URL=http://localhost:8081 DB_HOST=localhost DB_PORT=6432 SMTP_SERVER=localhost SMTP_PORT=1025 SMTP_LOGIN= SMTP_PASSWORD= cabal run op-server

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
