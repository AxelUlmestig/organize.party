-- Revert events:schemas/email from pg

BEGIN;

  drop schema if exists email;

COMMIT;
