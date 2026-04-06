-- Revert events:email/tables/emails from pg

BEGIN;

  drop table email.emails;

COMMIT;
