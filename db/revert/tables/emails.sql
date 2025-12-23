-- Revert events:tables/emails from pg

BEGIN;

  drop table if exists emails;

COMMIT;
