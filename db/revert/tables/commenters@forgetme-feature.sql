-- Revert events:tables/commenters from pg

BEGIN;

  drop table if exists commenters;

COMMIT;
