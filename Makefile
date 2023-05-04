.PHONY: start-dev-backend
start-dev-backend:
	docker compose up -d db
	DB_HOST=localhost DB_PORT=5433 SMTP_SERVER=localhost SMTP_PORT=1111 SMTP_LOGIN=user SMTP_PASSWORD=password cabal run

.PHONY: build-frontend
build-frontend:
	(cd frontend && elm make src/Main.elm)

.PHONY: access-database
access-database:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/events
