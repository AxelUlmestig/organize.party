-- Deploy events:schemas/email to pg

BEGIN;

  create schema if not exists email;

COMMIT;
