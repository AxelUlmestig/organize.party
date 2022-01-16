#!/bin/sh

NUKE_DB=$(cat <<-SQL
  drop schema public cascade;
  create schema public;
  grant all on schema public to current_user;
  grant all on schema public to public;
SQL
)

psql postgres://postgres:postgres@localhost:5433/events -c "$NUKE_DB"

psql postgres://postgres:postgres@localhost:5433/events \
  -f db/tables.sql
