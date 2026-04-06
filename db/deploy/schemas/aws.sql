-- Deploy events:schemas/aws to pg

BEGIN;

  create schema if not exists aws;

COMMIT;
