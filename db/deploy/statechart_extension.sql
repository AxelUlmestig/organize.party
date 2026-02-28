-- Deploy events:statechart_extension to pg

BEGIN;

  create extension if not exists pg_statecharts cascade;

COMMIT;
