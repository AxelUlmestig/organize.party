-- Revert events:email/tables/emails from pg

BEGIN;

  drop table if exists email.emails;

COMMIT;
