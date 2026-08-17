include .env
export

.PHONY: deploy-database
deploy-database:
	docker compose up -d pgbouncer
	./scripts/wait-for-db.sh
	docker compose exec db sqitch --chdir db deploy --mode change --verify

.PHONY: start-dev-webapi
start-dev-webapi: deploy-database
	cabal run op-webapi

.PHONY: start-dev-worker
start-dev-worker: deploy-database
	docker compose up -d mailhog
	cabal run op-worker

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
