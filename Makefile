.PHONY: deploy-database
deploy-database:
	docker compose up -d pgbouncer
	./scripts/wait-for-db.sh
	docker compose exec db sqitch --chdir db deploy --mode change --verify

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
	./scripts/access-database.sh

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
	docker compose -f docker-compose-prod.yml up -d db
	./scripts/wait-for-db.sh docker-compose-prod.yml
	docker compose -f docker-compose-prod.yml exec db sqitch --chdir db deploy --verify
	docker compose -f docker-compose-prod.yml up --force-recreate -d webapi worker

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
	docker compose up -d filehost
	cd frontend/test && npx playwright test --headed test.spec.ts --project chromium

.PHONY: lint
lint:
	hlint -X QuasiQuotes -X OverloadedRecordDot .

define GEN_CHARTS_QUERY
	select fsm.gen_statechart_sqitch_migrations(
		source_path => '/repo/db/statechart',
		sqitch_plan_file_path => '/repo/db/sqitch.plan',
		recursive => true,
		file_permission_666 => true
	);
endef

export GEN_CHARTS_QUERY
.PHONY: gen-charts
gen-charts:
	sudo chmod 666 db/sqitch.plan
	docker compose exec db psql postgres://postgres:postgres@pgbouncer:6432/events -c "$$GEN_CHARTS_QUERY"
