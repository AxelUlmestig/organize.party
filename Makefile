-include .env
export

# --- Fogpipe Cloud deployment ---------------------------------------------
#
# ORG is the organization's opaque id — the one image paths are built from,
# not its readable name. It is read from your login when you belong to exactly
# one organization, so there is usually nothing to pass. TAG defaults to the
# current commit.
#
# Secrets (SMTP, and an off-platform backup target if you want one) go in
# infra/secrets.auto.tfvars, which tofu loads on its own and git ignores.
# infra/secrets.auto.tfvars.example is the template.

DEPLOY_GOALS := deploy project images apply plan migrate logs
ifneq ($(filter $(DEPLOY_GOALS),$(MAKECMDGOALS)),)
ORG ?= $(shell ./scripts/fpcloud-org.sh)
endif
TAG ?= $(shell git rev-parse --short HEAD)

REPO := registry.cloud.fogpipe.com/$(ORG)/organizeparty
TF    := tofu -chdir=infra
TFVAR := -var org=$(ORG) -var image_tag=$(TAG)

require-org:
	@test -n "$(ORG)" || { echo "could not resolve the organization — see above, or pass ORG=<org-id>"; exit 1; }

# The whole deployment. Ordering is not cosmetic: the registry refuses a push
# to a repository path no project owns, so the project exists before the
# images do — and the apps cannot be created before their images are pushed.
.PHONY: deploy
deploy: project images apply migrate

.PHONY: project
project: require-org
	$(TF) init -input=false
	$(TF) apply $(TFVAR) -target=fpcloud_project.organizeparty

.PHONY: images
images: require-org
	fpcloud registry login
	docker build -f op-webapi/Dockerfile -t $(REPO)/webapi:$(TAG) .
	docker build -f op-worker/Dockerfile -t $(REPO)/worker:$(TAG) .
	docker push $(REPO)/webapi:$(TAG)
	docker push $(REPO)/worker:$(TAG)

.PHONY: apply
apply: require-org
	$(TF) apply $(TFVAR)

.PHONY: plan
plan: require-org
	$(TF) plan $(TFVAR)

# The database is cluster-internal, so the migrations run through a tunnel.
.PHONY: migrate
migrate: require-org
	ORG=$(ORG) ./scripts/deploy-migrations-fpcloud.sh

.PHONY: logs
logs: require-org
	fpcloud app logs webapi --org $(ORG) --project organizeparty --since 1h --follow

# --- Local development ----------------------------------------------------

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
